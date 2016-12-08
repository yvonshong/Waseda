/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2011, VU University Amsterdam

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

:- module(pdt_console,
	  [ pdt_install_console/0
	  ]).
:- use_foreign_library(foreign(pdt_console)).

%%	pdt_install_console
%
%	Support  get_single_char/1  in  PDT    console.  This  predicate
%	modifies =user_input= and =user_output= as follows:
%
%	  - If single-char mode is enabled, write "ESC s" over the user
%	    output and then wait for two characters, returning the
%	    first.
%	  - Output is changed to emit ESC as ESC ESC.
%
%	This protocol was  designed  for  PDT   by  Lukas  Degener.  The
%	original implementation was partly in Prolog.   This is a full C
%	implementation, both for speed.

pdt_install_console :-
	pdt_wrap_console,
	set_stream(user_input, tty(true)),
	set_stream(user_output, tty(true)),
	set_prolog_flag(tty_control, true).
