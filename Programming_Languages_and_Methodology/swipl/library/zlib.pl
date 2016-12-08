/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
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

:- module(zlib,
	  [ zopen/3,			% +Stream, -ZStream, +Option
	    gzopen/3,			% +File, +Mode, -Stream
	    gzopen/4			% +File, +Mode, -Stream, +Options
	  ]).
:- use_module(library(shlib)).
:- use_module(library(error)).
:- use_module(library(apply)).

:- predicate_options(zopen/3, 3,
		     [ format(oneof([gzip,deflate])),
		       multi_part(boolean),
		       close_parent(boolean),
		       level(between(0,9))
		     ]).
:- predicate_options(gzopen/4, 4,
		     [ pass_to(zopen/3, 3),
		       pass_to(system:open/4, 4)
		     ]).

/** <module> Zlib wrapper for SWI-Prolog

Read/write compressed data based on the zlib library.

@author Jan Wielemaker
@see	http://www.zlib.net/
@see	http://www.swi-prolog.org/packages/zlib.html
*/

:- use_foreign_library(foreign(zlib4pl)).
:- public zdebug/1.			% Set debug level

%%	gzopen(+File, +Mode, -Stream) is det.
%%	gzopen(+File, +Mode, -Stream, +Options) is det.
%
%	Open a file compatible with the  gzip   program.  Note that if a
%	file is opened in =append= mode,  a   second  gzip image will be
%	added to the end of the file.   The gzip standard defines that a
%	file can hold multiple  gzip  images   and  inflating  the  file
%	results in a concatenated stream of all inflated images.
%
%	Options are passed to open/4  and   zopen/3.  Default  format is
%	=gzip=.

gzopen(File, Mode, Stream) :-
	gzopen(File, Mode, Stream, []).

gzopen(File, Mode, Stream, Options) :-
	must_be(oneof([read,write,append]), Mode),
	partition(zoption, Options, ZOptions0, OpenOptions),
	merge_options(ZOptions0,
		      [ format(gzip),
			close_parent(true)
		      ], ZOptions),
	open(File, Mode, Stream0, OpenOptions),
	zopen(Stream0, Stream, ZOptions).


zoption(format(_)).
zoption(multi_part(_)).
zoption(level(_)).
