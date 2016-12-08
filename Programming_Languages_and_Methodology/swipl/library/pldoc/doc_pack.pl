/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2012, VU University Amsterdam

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

:- module(pldoc_pack,
	  [ doc_pack/1			% +Pack
	  ]).
:- use_module(library(prolog_pack)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).
:- use_module(doc_html).
:- use_module(doc_index).

/** <module> PlDoc for Prolog extension packs

This module profiles PlDoc support specific   to Prolog extension packs.
It extends the PlDoc web-browser with the ability to lists the installed
packs and provide an overview of  a   pack,  whether  loaded or not. The
predicate  doc_pack/1  can  be  used    to   generate  stand-alone  HTML
documentation for a pack.
*/

:- http_handler(pldoc(pack),     http_redirect(moved, pldoc('pack/')), []).
:- http_handler(pldoc('pack/'),  pldoc_pack, [prefix]).

%%	pldoc_pack(+Request)
%
%	HTTP handler that handles /pack/ in the PlDoc server. Without an
%	additional  path,  it  lists  the    installed  packs.  With  an
%	additional package name, it lists  the   content  of  a pack and
%	finally, /pack/<pack>/<file> can be used to get documentation or
%	the source of a pack file.

pldoc_pack(Request) :-
	memberchk(path_info(PackPath), Request),
	PackPath \== '', !,
	(   pack_path(Pack, PackFile, PackPath)
	->  list_pack(Pack, PackFile, Request)
	;   http_404([], Request)
	).
pldoc_pack(_Request) :-
	reply_html_page(
	    pldoc(packs),
	    title('Installed extension packs'),
	    \pack_page([])).

pack_path(Pack, PackFile, PackPath) :-
	sub_atom(PackPath, B, _, A, /), !,
	sub_atom(PackPath, 0, B, _, Pack),
	sub_atom(PackPath, _, A, 0, PackFile).

pack_page(Options) -->
	html_requires(pldoc),
	object_page_header(-, Options),
	html([ h1('Installed extension packs'),
	       p([ 'The following extension packages are installed in ',
		   'the this Prolog system.  Other packages can be found at ',
		   a(href('http://www.swi-prolog.org/pack/list'),
		     'the SWI-Prolog website')
		 ]),
	       \pack_table(Options)
	     ]).


%%	pack_table(+Options)// is det.
%
%	Generate a table with installed packages

pack_table(_Options) -->
	{ findall(Pack, pack_property(Pack, directory(_)), Packs0),
	  sort(Packs0, Packs)
	},
	html(table(class(packs),
		   [ tr([th('Pack'), th('Version'), th('Title')])
		   | \packs(Packs)
		   ])).

packs([]) --> [].
packs([H|T]) --> pack(H), packs(T).

pack(Pack) -->
	{ uri_encoded(path, Pack, HREF),
	  pack_property(Pack, version(Version)),
	  (   pack_property(Pack, title(Title))
	  ->  true
	  ;   Title = '<no title>'
	  )
	},
	html(tr([ td(class(pack_name),    a(href(HREF+'/'), Pack)),
		  td(class(pack_version), Version),
		  td(class(pack_title),   Title)
		])).


%%	list_pack(+Pack, +PackFile, +Request)
%
%	List a directory or file in a pack

list_pack(Pack, '', _) :- !,
	reply_html_page(
	    pldoc(pack),
	    title('Documentation for pack ~w'-[Pack]),
	    \pack_doc(Pack)).
list_pack(Pack, File, Request) :-
	pack_property(Pack, directory(PackDir)),
	directory_file_path(PackDir, File, Path0),
	absolute_file_name(Path0, Path),	% Canonical
	sub_atom(Path, 0, _, _, PackDir),
	pldoc_http:doc_reply_file(Path, Request).

pack_doc(Pack) -->
	{ pack_property(Pack, directory(PackDir)),
	  pack_title(Pack, Title),
	  findall(O, pack_option(Pack, O), Options)
	},
	dir_index(PackDir,
		  [ if(true),
		    recursive(true),
		    title(Title)
		  | Options
		  ]).


		 /*******************************
		 *	  STAND ALONE DOCS	*
		 *******************************/

%%	doc_pack(+Pack)
%
%	Generate stand-alone documentation for  the   package  Pack. The
%	documentation is generated in a directory =doc= inside the pack.
%	The  index  page  consists  of  the    content  of  =readme=  or
%	=|readme.txt|= in the main directory of the pack and an index of
%	all files and their public predicates.

doc_pack(Pack) :-
	pack_property(Pack, directory(PackDir)),
	pack_title(Pack, PackTitle),
	findall(O, pack_option(Pack, O), Options),
	directory_file_path(PackDir, prolog, SourceDir),
	directory_file_path(PackDir, doc, DocDir),
	doc_save(SourceDir,
		 [ title(PackTitle),
		   doc_root(DocDir),
		   if(true),
		   recursive(true)
		 | Options
		 ]).

pack_title(Pack, PackTitle) :-
	pack_property(Pack, title(Title)), !,
	format(atom(PackTitle), 'Pack ~w -- ~w', [Pack, Title]).
pack_title(Pack, PackTitle) :-
	format(atom(PackTitle), 'Pack ~w', [Pack]).

pack_option(Pack, Option) :-
	pack_option(Option),
	pack_property(Pack, Option).

pack_option(readme(_)).
pack_option(todo(_)).
