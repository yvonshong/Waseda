/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2011-2013, University of Amsterdam
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

:- module(pldoc_process,
	  [ doc_comment/4,		% ?Object, ?Pos, ?Summary, ?Comment
	    doc_file_has_comments/1,	% +File
	    is_structured_comment/2,	% +Comment, -Prefixes
	    parse_comment/3,		% +Comment, +FilePos, -Parsed
	    process_comments/3,		% +Comments, +StartTermPos, +File
	    doc_file_name/3		% +Source, -Doc, +Options
	  ]).

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

user:file_search_path(pldoc, library(pldoc)).

:- load_files([ pldoc(doc_register),
		pldoc(doc_modes),
		pldoc(doc_wiki),
		library(debug),
		library(option),
		library(lists),
		library(apply),
		library(operators),
		library(prolog_source)
	      ],
	      [ silent(true),
		if(not_loaded)
	      ]).

/** <module> Process source documentation
The pldoc module processes structured comments in Prolog source files into
well formatted HTML documents.

@author  Jan Wielemaker
@license GPL
*/

:- predicate_options(doc_file_name/3, 3,
		     [ format(oneof([html,tex]))
		     ]).

%%	prolog:predicate_summary(+PI, -Summary) is semidet.
%
%	Provide    predicate    summaries    to     the    XPCE    class
%	=prolog_predicate=, used by the IDE tools.

:- multifile
	prolog:predicate_summary/2.	% ?PI, -Summary


%%	is_structured_comment(+Comment:string,
%%			      -Prefixes:list(codes)) is semidet.
%
%	True if Comment is a structured comment that should use Prefixes
%	to extract the plain text using indented_lines/3.

is_structured_comment(Comment, Prefixes) :-
	is_structured_comment(Comment, Prefixes, _Style).

is_structured_comment(_Pos-Comment, Prefixes, Style) :- !,
	is_structured_comment(Comment, Prefixes, Style).
is_structured_comment(Comment, Prefixes, Style) :-
	is_list(Comment), !,
	(   phrase(structured_comment(Prefixes, Style), Comment, _)
	->  true
	).
is_structured_comment(Comment, Prefixes, Style) :-
	atom_string(CommentA, Comment),
	structured_command_start(Start, Prefixes, Style),
	sub_atom(CommentA, 0, Len, _, Start), !,
	sub_atom(CommentA, Len, 1, _, Space),
	char_type(Space, space),
	(   Style == block
	->  true
	;   \+ blanks_to_nl(CommentA)
	).

structured_command_start('%%',  ["%"], percent_percent).	% Deprecated
structured_command_start('%!',  ["%"], percent_bang).		% New style
structured_command_start('/**', ["/**", " *"], block).		% block

blanks_to_nl(CommentA) :-
	sub_atom(CommentA, At, 1, _, Char),
	At >= 2,
	(   char_type(Char, end_of_line)
	->  !
	;   (   char_type(Char, space)
	    ;	Char == '%'
	    )
	->  fail
	;   !, fail
	).
blanks_to_nl(_).

%%	structured_comment(-Prefixes:list(codes), -Style) is semidet.
%
%	Grammar rule version of the above.  Avoids the need for
%	conversion.

structured_comment(["%"], percent_percent) -->
	"%%", space,
	\+ separator_line.
structured_comment(["%"], percent_bang) -->
	"%!", space.
structured_comment(Prefixes, block) -->
	"/**", space,
	{ Prefixes = ["/**", " *"]
	}.

space -->
	[H],
	{ code_type(H, space) }.

%%	separator_line// is semidet.
%
%	Matches a line like %% SWI or %%%%%%%%%%%%%%%%%%%%%%%%%, etc.

separator_line -->
	string(S), "\n", !,
	{   maplist(blank_or_percent, S)
	;   contains(S, " SWI ")
	;   contains(S, " SICStus ")
	;   contains(S, " Mats ")
	}.

string([]) --> [].
string([H|T]) --> [H], string(T).

blank_or_percent(0'%) :- !.
blank_or_percent(C) :-
	code_type(C, space).

contains(Haystack, Needle) :-
	string_codes(Needle, NeedleCodes),
	append(_, Start, Haystack),
	append(NeedleCodes, _, Start), !.


%%	doc_file_name(+Source:atom, -Doc:atom, +Options:list) is det.
%
%	Doc is the name of the file for documenting Source.
%
%	@param Source	Prolog source to be documented
%	@param Doc	the name of the file documenting Source.
%	@param Options	Option list:
%
%			* format(+Format)
%			Output format.  One of =html= or =tex=
%
%	@error	permission_error(overwrite, Source)

doc_file_name(Source, Doc, Options) :-
	option(format(Format), Options, html),
	file_name_extension(Base, _Ext, Source),
	file_name_extension(Base, Format, Doc),
	(   Source == Doc
	->  throw(error(permission_error(overwrite, Source), _))
	;   true
	).

%%	doc_file_has_comments(+Source:atom) is semidet.
%
%	True if we have loaded comments from Source.

doc_file_has_comments(Source) :-
	source_file_property(Source, module(M)),
	locally_defined(M:'$pldoc'/4),
	M:'$pldoc'(_, _, _, _).


%%	doc_comment(?Objects, -Pos,
%%		    -Summary:string, -Comment:string) is nondet.
%
%	True if Comment is the  comment   describing  object. Comment is
%	returned as a string object  containing   the  original from the
%	source-code.  Object is one of
%
%		* Name/Arity
%		Predicate indicator
%
%		* Name//Arity
%		DCG rule indicator.  Same as Name/Arity+2
%
%		* module(ModuleTitle)
%		Comment appearing in a module.
%
%	If Object is  unbound  and  multiple   objects  share  the  same
%	description, Object is unified with a   list  of terms described
%	above.
%
%	@param Summary	First sentence.  Normalised spacing.
%	@param Comment	Comment string from the source-code (untranslated)

doc_comment(Object, Pos, Summary, Comment) :-
	var(Object), !,
	locally_defined(M:'$pldoc'/4),
	M:'$pldoc'(Obj, Pos, Summary, Comment),
	qualify(M, Obj, Object0),
	(   locally_defined(M:'$pldoc_link'/2),
	    findall(L, M:'$pldoc_link'(L, Obj), Ls), Ls \== []
	->  maplist(qualify(M),	Ls, QLs),
	    Object = [Object0|QLs]
	;   Object = Object0
	).
doc_comment(M:Object, Pos, Summary, Comment) :- !,
	locally_defined(M:'$pldoc'/4),
	(   M:'$pldoc'(Object, Pos, Summary, Comment)
	;   locally_defined(M:'$pldoc_link'/2),
	    M:'$pldoc_link'(Object, Obj2),
	    M:'$pldoc'(Obj2, Pos, Summary, Comment)
	).
doc_comment(Name/Arity, Pos, Summary, Comment) :-
	system_module(M),
	doc_comment(M:Name/Arity, Pos, Summary, Comment).


locally_defined(M:Name/Arity) :-
	current_module(M),
	current_predicate(M:Name/Arity),
	functor(Head, Name, Arity),
	\+ predicate_property(M:Head, imported_from(_)).


qualify(M, H, H) :- system_module(M), !.
qualify(M, H, H) :- sub_atom(M, 0, _, _, $), !.
qualify(M, H, M:H).

system_module(user).
system_module(system).


%	Make the summary available to external tools on plugin basis.

prolog:predicate_summary(PI, Summary) :-
	doc_comment(PI, _, Summary, _).


		 /*******************************
		 *	CALL-BACK COLLECT	*
		 *******************************/

%%	process_comments(+Comments:list, +TermPos, +File) is det.
%
%	Processes comments returned by read_term/3 using the =comments=
%	option.  It creates clauses of the form
%
%		* '$mode'(Head, Det)
%		* '$pldoc'(Id, Pos, Summary, Comment)
%		* '$pldoc_link'(Id0, Id)
%
%	where Id is one of
%
%		* module(Title)
%		Generated from /** <module> Title */
%		* Name/Arity
%		Generated from Name(Arg, ...)
%		* Name//Arity
%		Generated from Name(Arg, ...)//
%
%	@param Comments is a list Pos-Comment returned by read_term/3
%	@param TermPos is the start-location of the actual term
%	@param File is the file that is being loaded.

process_comments([], _, _).
process_comments([Pos-Comment|T], TermPos, File) :-
	(   Pos @> TermPos		% comments inside term
	->  true
	;   process_comment(Pos, Comment, File),
	    process_comments(T, TermPos, File)
	).

process_comment(Pos, Comment, File) :-
	is_structured_comment(Comment, Prefixes, Style), !,
	stream_position_data(line_count, Pos, Line),
	FilePos = File:Line,
	process_structured_comment(FilePos, Comment, Prefixes, Style).
process_comment(_, _, _).

%%	parse_comment(+Comment, +FilePos, -Parsed) is semidet.
%
%	True when Comment is a  structured   comment  and  Parsed is its
%	parsed representation. Parsed is a list of the following terms:
%
%	  * section(Id, Title, Comment)
%	  Generated from /** <module> Title Comment */ comments.
%	  * predicate(PI, Summary, Comment)
%	  Comment for predicate PI
%	  * link(FromPI, ToPI)
%	  Indicate that FromPI shares its comment with ToPI.  The actual
%	  comment is in ToPI.
%	  * mode(Head, Determinism)
%	  Mode declaration.  Head is a term with Mode(Type) terms and
%	  Determinism describes the associated determinism (=det=,
%	  etc.).

parse_comment(Comment, FilePos, Parsed) :-
	is_structured_comment(Comment, Prefixes), !,
	compile_comment(Comment, FilePos, Prefixes, Parsed).


%%	process_structured_comment(+FilePos,
%%				   +Comment:string,
%%				   +Prefixed:list,
%%				   +Style) is det.

process_structured_comment(FilePos, Comment, _, _) :- % already processed
	prolog_load_context(module, M),
	locally_defined(M:'$pldoc'/4),
	catch(M:'$pldoc'(_, FilePos, _, Comment), _, fail), !.
process_structured_comment(FilePos, Comment, Prefixes, Style) :-
	catch(compile_comment(Comment, FilePos, Prefixes, Compiled), E,
	      comment_warning(Style, E)),
	maplist(store_comment(FilePos), Compiled).
process_structured_comment(FilePos, Comment, _Prefixes, Style) :-
	comment_style_warning_level(Style, Level),
	print_message(Level,
		      pldoc(invalid_comment(FilePos, Comment))).

comment_style_warning_level(percent_percent, silent) :- !.
comment_style_warning_level(_, warning).

%%	comment_warning(+Style, +Error) is failure.
%
%	Print a warning  on  structured  comments   that  could  not  be
%	processed. Since the recommended magic   sequence is now =|%!|=,
%	we remain silent about comments that start with =|%%|=.

comment_warning(Style, E) :-
	comment_style_warning_level(Style, Level),
	print_message(Level, E),
	fail.

%%	compile_comment(+Comment, +FilePos, +Prefixes, -Compiled) is semidet.
%
%	Compile structured Comment into a list   of  terms that describe
%	the comment.
%
%	@see parse_comment/3 for the terms in Compiled.

compile_comment(Comment, FilePos, Prefixes, Compiled) :-
	string_codes(Comment, CommentCodes),
	indented_lines(CommentCodes, Prefixes, Lines),
	(   section_comment_header(Lines, Header, _RestLines)
	->  Header = \section(Type, Title),
	    Id =.. [Type,Title],
	    Compiled = [section(Id, Title, Comment)]
	;   prolog_load_context(module, Module),
	    process_modes(Lines, Module, FilePos, Modes, _, RestLines)
	->  maplist(compile_mode, Modes, ModeDecls),
	    modes_to_predicate_indicators(Modes, AllPIs),
	    decl_module(AllPIs, M, [PI0|PIs]),
	    maplist(link_term(M:PI0), PIs, Links),
	    summary_from_lines(RestLines, Codes),
	    string_codes(Summary, Codes),
	    append([ ModeDecls,
		     [ predicate(M:PI0, Summary, Comment) ],
		     Links
		   ], Compiled)
	), !.


store_comment(Pos, section(Id, Title, Comment)) :- !,
	compile_clause('$pldoc'(Id, Pos, Title, Comment), Pos).
store_comment(Pos, predicate(M:PI, Summary, Comment)) :- !,
	compile_clause(M:'$pldoc'(PI, Pos, Summary, Comment), Pos).
store_comment(Pos, link(PI, M:PI0)) :- !,
	compile_clause(M:'$pldoc_link'(PI, PI0), Pos).
store_comment(Pos, mode(Head, Det)) :- !,
	compile_clause('$mode'(Head, Det), Pos).
store_comment(_, Term) :-
	type_error(pldoc_term, Term).

link_term(To, From, link(From,To)).

decl_module([], M, []) :-
	(   var(M)
	->  prolog_load_context(module, M)
	;   true
	).
decl_module([H0|T0], M, [H|T]) :-
	(   H0 = M1:H
	->  M = M1
	;   H = H0
	),
	decl_module(T0, M, T).


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile
	prolog:message//1.

prolog:message(pldoc(invalid_comment(File:Line, Comment))) -->
	[ '~w:~d: PlDoc: failed to process structured comment:~n~s~n'-
		[File, Line, Comment]
	].
