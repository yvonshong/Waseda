/*  $Id: 209010073bd8ff4a785725933c95fca8cb77b855 $

    Part of SWI-Prolog

    Author:        Mike Elston
                   Matt Lilley
    E-mail:        matt.s.lilley@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2014, Mike Elston, Matt Lilley

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

    PostgreSQL is a trademark of the PostgreSQL Global Development Group.
    Microsoft, SQL Server, and Windows are either registered trademarks or
    trademarks of Microsoft Corporation in the United States and/or other
    countries.
    SQLite is a registered trademark of Hipp, Wyrick & Company, Inc in the United
    States.
    All other trademarks or registered trademarks are the property of their
    respective owners.
*/


:-module(cql_hooks, [application_value_to_odbc_value_hook/7,
                     odbc_value_to_application_value_hook/7,
                     cql_update_history_hook/16
                     ]).

:-multifile(odbc_value_to_application_value_hook/7).
:-multifile(application_value_to_odbc_value_hook/7).


%%      cql_update_history_hook(+Schema,
%%                              +TableName,
%%                              +AttributeName,
%%                              +PrimaryKeyAttributeName,
%%                              +PrimaryKeyValue,
%%                              +ApplicationValueBefore,
%%                              +ApplicationValueAfter,
%%                              +AccessToken,
%%                              +UserId,
%%                              +UserIpAddress,
%%                              +TransactionId,
%%                              +TransactionTimestamp,
%%                              +ThreadId,
%%                              +Spid,
%%                              +Connection,
%%                              +Goal).
%
%       Use this hook predicate to actually record database attribute value changes.
%
%       You are free to let this predicate fail or raise an exception - the
%       database layer will ignore both of these eventualities.
%
%       @param Schema <atom>
%       @param TableName <atom> (lower case)
%       @param AttributeName <atom> (lower case)
%       @param PrimaryKeyAttributeName <atom> (lower case)
%       @param PrimaryKeyValue <int>
%       @param ApplicationValueBefore <domain dependent>
%       @param ApplicationValueAfter <domain dependent>
%       @param AccessToken <atom>
%       @param UserId <atom>
%       @param UserIpAddress <atom>
%       @param TransactionId <atom>
%       @param TransactionTimestamp <t7/7>
%       @param ThreadId <atom>
%       @param Spid <int>
%       @param Connection <opaque>
%       @param Goal <goal term> The goal passed to pri_db_trans
:-multifile(cql_update_history_hook/16).
