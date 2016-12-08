/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2013, University of Amsterdam
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(pldoc_colours,
	  [ colour_fragments/2		% +Source, -Fragments
	  ]).
:- use_module(library(prolog_xref)).
:- use_module(library(prolog_source)).
:- use_module(library(prolog_colour)).

/** <module> Source colouring support

Provide   hierarchical   colouring   information   on     top   of   the
library(prolog_colour). We need  ordered   hierarchical  information  to
create HTML fragments.
*/

:- thread_local
	fragment/3.			% Start, Length, Class

:- create_prolog_flag(xref, false, [type(boolean)]).

%%	colour_fragments(+In, -Fragments:list) is det.
%
%	Create a list of colour fragments from In.
%
%	@param Fragments	List of fragment(Start, End, Class)

colour_fragments(Source, Fragments) :-
	F = fragment(_,_,_),
	retractall(F),
	prolog_canonical_source(Source, SourceID),
	xref_source(SourceID, [silent(true)]),
	setup_call_cleanup(
	    prolog_open_source(SourceID, Stream),
	    prolog_colourise_stream(Stream, SourceID, assert_fragment),
	    prolog_close_source(Stream)),
	findall(F, retract(F), Fragments0),
	sort(Fragments0, Fragments1),
	fragment_hierarchy(Fragments1, Fragments).

assert_fragment(Class, Start, Length) :-
	End is Start+Length,
	assert(fragment(Start, End, Class)).


%%	fragment_hierarchy(+Fragments, -Hierarchy) is det.
%
%	Translate   list   of   fragment(Start,     End,   Class)   into
%	fragment(Start, End, Class, SubFragments).
%
%	@tbd	Detect improper nesting.  How to handle?

fragment_hierarchy([], []).
fragment_hierarchy([fragment(S,E,C)|Rest0], [fragment(S,E,C,Sub)|Rest]) :-
	sub_fragments(Rest0, E, Sub, Rest1),
	fragment_hierarchy(Rest1, Rest).

sub_fragments([], _, [], []).
sub_fragments([F|R0], End, Sub, Rest) :-
	F = fragment(SF,EF,C),
	(   EF =< End
	->  Sub = [fragment(SF,EF,C,FSub)|RSub],
	    sub_fragments(R0, EF, FSub, R1),
	    sub_fragments(R1, End, RSub, Rest)
	;   Sub = [],
	    Rest = [F|R0]
	).
