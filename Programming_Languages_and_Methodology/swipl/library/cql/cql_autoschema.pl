/*  $Id: ec8894e67cf3b9fa4f0bb198dcfd628367ac72c7 $

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

:-module(cql_autoschema,
         []).

% Database metadata API  ---------------------------------------------------------------------------------------------------------------
/*
%%      dbms(+Schema, -DBMSName).
%       Determine the DBMS for a given Schema.
%       Can be autoconfigured.
:-discontiguous(dbms/2).

%%      odbc_data_type(+Schema, +TableSpec, +ColumnName, ?OdbcDataType)
%       OdbcDataType must be a native SQL datatype, such as varchar(30) or decimal(10, 5)
%       Can be autoconfigured.
:-discontiguous(odbc_data_type/4).

%%      primary_key_column_name(+Schema, +TableName, -PrimaryKeyAttributeName).
%       Can be autoconfigured.
:-discontiguous(primary_key_column_name/3).

%%      database_attribute(?EntityType:table/view, ?Schema:atom, ?EntityName:atom, ?ColumnName:atom, ?DomainOrNativeType:atom, ?AllowsNulls:allows_nulls(true/false), ?IsIdentity:is_identity(true/false), ?ColumnDefault) is nondet.
%       Can be autoconfigured.
:-discontiguous(database_attribute/8).

%%      database_attribute(?DomainName:atom, ?OdbcDataType) is nondet.
%       Can be autoconfigured.
:-discontiguous(database_domain/2).

%%      routine_return_type(?Schema:atom, ?EntityName:atom, ?OdbcType).
%       Can be autoconfigured
:-discontiguous(routine_return_type/3).

%%      database_constraint(?Schema:atom, ?EntityName:atom, ?ConstraintName:atom, ?Constraint) is nondet.
%       Constraint is one of:
%          * primary_key(ColumnNames:list)
%          * foreign_key(ForeignTableName:atom, ForeignColumnNames:list, ColumnNames:list)
%          * unique(ColumnNames:list)
%          * check(CheckClause)
%       In theory this can be autoconfigured too, but I have not written the code for it yet
:-discontiguous(database_constraint/4).
*/

:-use_module(library(cql/cql_database)).
:-use_module(library(dcg/basics)).

% Automatic schema generation ---------------------------------------------------------------------------------------------------------------
% Works with PostgreSQL and SQLite. Could be extended to work with another DBMS easily enough

schema_fact(Schema, DBMS, dbms(Schema, DBMS)).

schema_fact(Schema, 'PostgreSQL', Fact):-
        odbc_connection_call(Schema, Connection, odbc_query(Connection, 'SELECT table_name, column_name, data_type, character_maximum_length, numeric_precision, numeric_scale, domain_name, is_nullable, column_default FROM INFORMATION_SCHEMA.columns WHERE table_schema = \'public\'', row(EntityName, ColumnName, NativeType, CharacterMaximumLength, NumericPrecision, NumericScale, DomainType, IsNullable, ColumnDefault))),
        ( IsNullable == '1' -> AllowsNulls = allows_nulls(true) ; AllowsNulls = allows_nulls(false) ),
        map_native_type(NativeType, CharacterMaximumLength, NumericPrecision, NumericScale, Mapped),
        ( DomainType \== {null} -> DomainOrNativeType = domain(DomainType) ; DomainOrNativeType = native_type(Mapped) ),
        IsIdentity = is_identity(false),
        ( Fact = database_attribute(table, Schema, EntityName, ColumnName, DomainOrNativeType, AllowsNulls, IsIdentity, ColumnDefault)
        ; Fact = odbc_data_type(Schema, EntityName, ColumnName, Mapped)
        ).

schema_fact(Schema, 'PostgreSQL', primary_key_column_name(Schema, EntityName, ColumnName)):-
        odbc_connection_call(Schema, Connection, odbc_query(Connection, 'SELECT kcu.table_name, column_name FROM INFORMATION_SCHEMA.key_column_usage KCU INNER JOIN INFORMATION_SCHEMA.table_constraints tc ON(tc.table_name = kcu.table_name AND tc.constraint_type = \'PRIMARY KEY\') WHERE kcu.table_schema = \'public\'', row(EntityName, ColumnName))).

schema_fact(Schema, 'PostgreSQL', database_domain(DomainName, Mapped)):-
        odbc_connection_call(Schema, Connection, odbc_query(Connection, 'SELECT domain_name, data_type, character_maximum_length, numeric_precision, numeric_scale FROM INFORMATION_SCHEMA.domains WHERE domain_schema = \'public\'', row(DomainName, NativeType, CharacterMaximumLength, NumericPrecision, NumericScale))),
        map_native_type(NativeType, CharacterMaximumLength, NumericPrecision, NumericScale, Mapped).

schema_fact(Schema, 'PostgreSQL', routine_return_type(Schema, RoutineName, Mapped)):-
        odbc_connection_call(Schema, Connection, odbc_query(Connection, 'SELECT data_type, character_maximum_length, numeric_precision, numeric_scale FROM INFORMATION_SCHEMA.routines WHERE routine_schema = \'public\'', row(RoutineName, NativeType, CharacterMaximumLength, NumericPrecision, NumericScale))),
        map_native_type(NativeType, CharacterMaximumLength, NumericPrecision, NumericScale, Mapped).

schema_fact(Schema, 'SQLite', Fact):-
        findall(EntityName,
                odbc_connection_call(Schema, Connection, odbc_query(Connection, 'SELECT name FROM sqlite_master WHERE type = \'table\'', row(EntityName))),
                EntityNames),
        member(EntityName, EntityNames),
        format(atom(Pragma), 'PRAGMA table_info(~w)', [EntityName]),
        odbc_connection_call(Schema, Connection, odbc_query(Connection, Pragma, Row)),
        Row = row(_CID, ColumnName, ColumnTypeAtom, NotNullable, ColumnDefault, IsPkMember),
        ( NotNullable == 0 ->
            AllowsNulls = allows_nulls(true)
        ; otherwise->
            AllowsNulls = allows_nulls(false)
        ),
        ColumnTypeAtom \== '', % sqlite_master contains sequence tables too, sadly. This excludes them
        ( sqlite_type(ColumnTypeAtom, ColumnType)->
            true
        ; otherwise->
            writeln(bad_type(EntityName, ColumnName, ColumnTypeAtom))
        ),
        IsIdentity = is_identity(false), % FIXME
        % SQLite does not support domains. Everything is a native_type
        ( ColumnType == datetime ->
            % FIXME: What is going on here?
            OdbcDataType = timestamp
        ; otherwise->
            OdbcDataType = ColumnType
        ),
        ( Fact = database_attribute(table, Schema, EntityName, ColumnName, native_type(ColumnType), AllowsNulls, IsIdentity, ColumnDefault)
        ; Fact = odbc_data_type(Schema, EntityName, ColumnName, OdbcDataType)
        ; IsPkMember == 1, Fact = primary_key_column_name(Schema, EntityName, ColumnName)
        ).


map_native_type('character varying', N, _, _, varchar(N)):- !.
map_native_type('decimal', _, P, S, decimal(P, S)):- !.
map_native_type(X, _, _, _, X).

sqlite_type(Atom, Type):-
        atom_codes(Atom, Codes),
        sqlite_type_1(Type, Codes, []).
sqlite_type_1(decimal(P, S))-->
        ( "DECIMAL" ; "decimal" ; "Decimal"),
        !,
        whitespace, "(", whitespace, number(P), whitespace, ",", whitespace, number(S), whitespace,  ")".

sqlite_type_1(varchar(N))-->
        ( "VARCHAR" ; "varchar" ; "Varchar"),
        !,
        whitespace, "(", whitespace, number(N), whitespace, ")".

sqlite_type_1(varbinary(max))-->
        ( "VARBINARY" ; "varbinary" ; "Varbinary"), !.

sqlite_type_1(longvarchar)-->
        ( "LONGVARCHAR" ; "longvarchar" ; "Longvarchar"), !.

sqlite_type_1(integer)-->
        ( "INTEGER" ; "integer" ; "Integer"), !.

sqlite_type_1(datetime)-->
        ( "DATETIME" ; "datetime" ; "Datetime"), !.

sqlite_type_1(bigint)-->
        ( "BIGINT" ; "bigint" ; "Bigint"), !.

sqlite_type_1(smallint)-->
        ( "SMALLINT" ; "smallint" ; "Smallint"), !.


whitespace--> [C], {code_type(C, white)}, !, whitespace.
whitespace--> [].


user:term_expansion(:-build_schema(Schema), Facts):-
        odbc_connection_call(Schema, Connection, odbc_get_connection(Connection, dbms_name(DBMS))),
        setof(cql:Fact,
              schema_fact(Schema, DBMS, Fact),
              Facts).
