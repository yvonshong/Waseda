/*  $Id: 5eb83e3ca8a4a6503a033aef1336c88c03155037 $

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



% For use with PostgreSQL. It is possible to use with SQL Server, but I dont know how to create
% users from the command prompt.
% CREATE DATABASE cql_demo
% Then psql cql_demo
% CREATE EXTENSION "uuid-ossp"
% CREATE TABLE cql_table_1(cql_table_1_pk SERIAL PRIMARY KEY, varchar_column varchar(30))
% CREATE TABLE cql_table_2(cql_table_2_pk SERIAL PRIMARY KEY, varchar_column varchar(30), decimal_column DECIMAL(30,10))
:-module(cql_demo, [demo/0]).
:-use_module(library(cql/cql)).
:-cql_option(default_schema(seattle)).
:-cql_option(max_db_connections(10)).

% This is defined in cql_autoschema.pl and uses term_expansion to create the necessary clauses to drive the compiler. We do this in reverse - keep the metadata
% in code as facts, and have code to make the database we connect to conform. This allows us to keep the schema under source control. For a quick demo, though
% the auto-schema is probably more convenient.

:-use_module(library(cql/cql_autoschema)).
% Need to register a driver_string/1 (or a dsn(DSN, Username, Password) term). Not sure if there is a less obtrusive way to do this?
:-initialization(register_database_connection_details(seattle, driver_string('Driver={Sqlite3};Database=/tmp/cql.db')), now).
:-build_schema(seattle).


cql:application_value_to_odbc_value_hook(Type, _, _, _, _, Rational, Atom):-
        Type = decimal(_,S),
        format(atom(Atom), '~*f', [S, Rational]).

cql:odbc_value_to_application_value_hook(decimal(_,_), _, _, _, _, Value, Rational):-
        ( atom_prefix(Value, '.') ->
            atom_concat('0', Value, NumericAtom)
        ; atom_concat('-.', Rest, Value) ->
            atom_concat('-0.', Rest, NumericAtom)
        ; otherwise ->
            NumericAtom = Value
        ),
        atom_to_term(NumericAtom, Float, _),
        Rational is rationalize(Float).


% Also note that the empty list is a legacy of CQLv1. Ultimately I would like to change {[], .....} to be just {....}.
% For now, we are in a hurry, so it will have to stick around.

demo:-
        writeln('Delete all data'),
        cql_transaction(seattle, matt, delete_all_data),
        writeln('Insert some data'),
        cql_transaction(seattle, matt, insert_some_data),
        writeln('Select back data from table 1'),
        forall(simple_select(X),
               writeln(varchar_value=X)),
        writeln('Select back data with join'),
        forall(join_select(X),
               writeln(decimal_value=X)),
        writeln('Get data via aggregation'),
        forall(aggregate_select(X),
               writeln(sum_of_decimal_value=X)),
        writeln('Update data'),
        cql_transaction(seattle, matt, update_some_data(Count)),
        writeln(updated_rows=Count),
        writeln('Select back data with join again'),
        forall(join_select(X),
               writeln(decimal_value=X)).



simple_select(X):-
        {[], cql_table_1 :: [varchar_column-X]}.

join_select(X):-
        {[],
         cql_table_1 :: [varchar_column-Join]
        =*=
        cql_table_2 :: [varchar_column-Join,
                        decimal_column-X],
         order_by([+X])}.

aggregate_select(X):-
        ??? {[],
         cql_table_1 :: [varchar_column-Join]
        =*= % Inner join based on shared variables
        cql_table_2 :: [varchar_column-Join,
                        sum(decimal_column)-X]}.

update_some_data(Count):-
        {[],
         update(cql_table_2, [varchar_column-zap]),
         @ :: [decimal_column-X],
         X > 1 rdiv 4,
         X < 3 rdiv 4,
         row_count(Count)}.

delete_all_data:-
        {[], delete(cql_table_1, []), absence_of_where_restriction_is_deliberate},
        {[], delete(cql_table_2, []), absence_of_where_restriction_is_deliberate}.

insert_some_data:-
        {[], insert(cql_table_1, [varchar_column-foo])},
        {[], insert(cql_table_1, [varchar_column-bar])},
        {[], insert(cql_table_1, [varchar_column-baz])},
        {[], insert(cql_table_1, [varchar_column-qux])},
        % Unicode and rationals are supported, insofar as the underlying DB supports them
        {[], insert(cql_table_2, [varchar_column-これはテストです, decimal_column-3 rdiv 4])},
        {[], insert(cql_table_2, [varchar_column-english_message, decimal_column-4 rdiv 5])},
        {[], insert(cql_table_2, [varchar_column-bar, decimal_column-3 rdiv 8])},
        {[], insert(cql_table_2, [varchar_column-foo, decimal_column-1 rdiv 8])}.


splunge(GlAccountingDate):-
        ({[],
          se_lt_z :: [i-FromGlAccountingDate],
          FromGlAccountingDate =< GlAccountingDate} ->
           true
        ;
         true).


/*
 CREATE TABLE se_lt_x(x_pk INTEGER PRIMARY KEY AUTOINCREMENT, a VARCHAR(256), b VARCHAR(50), c INTEGER, d VARCHAR(50), e VARBINARY(50), f VARCHAR(50));
 CREATE TABLE se_lt_x1(x_pk INTEGER PRIMARY KEY AUTOINCREMENT, a VARCHAR(256), b VARCHAR(50), c INTEGER, d VARCHAR(50));
 CREATE TABLE se_lt_y(d VARCHAR(50), e VARCHAR(50), f INTEGER, inserted_ DATETIME, updated_ DATETIME);
 CREATE TABLE se_lt_z(g VARCHAR(50), h VARCHAR(50), i INTEGER, j VARCHAR(50), k VARCHAR(50), inserted_ DATETIME, updated_ DATETIME, inserted_by_ VARCHAR(50), updated_by_ VARCHAR(50), generation_ INTEGER, transaction_id_ VARCHAR(64));
 CREATE TABLE se_lt_types(se_types_pk INTEGER PRIMARY KEY AUTOINCREMENT, integer_ INTEGER, bigint_ BIGINT, tinyint_ SMALLINT, bit_ SMALLINT, varchar_ VARCHAR(50), varchar_max_ VARCHAR(8000), nvarchar_ VARCHAR(50), nvarchar_max_ VARCHAR(8000), varbinary_ VARBINARY, varbinary_max_ VARBINARY, t7_ DATETIME, datetime_ DATETIME, datetime2_ DATETIME, decimal_ DECIMAL(18,5), decimal_2 DECIMAL(9,2), boolean_ SMALLINT, inserted_ DATETIME, updated_ DATETIME, inserted_by_ VARCHAR(50), updated_by_ VARCHAR(50), generation_ INTEGER, transaction_id_ VARCHAR(64), encrypted VARCHAR(256));
 CREATE TABLE se_lt_n(n INTEGER, i INTEGER, j DECIMAL(18,4), k DECIMAL(18, 2));
 CREATE TABLE se_lt_history(schema_ VARCHAR(50), table_name VARCHAR(50), attribute_name VARCHAR(50), primary_key_attribute_name VARCHAR(50), primary_key_value INTEGER, application_value_before VARCHAR(512), application_value_after VARCHAR(512), access_token VARCHAR(50), user_id VARCHAR(50), user_ip_address VARCHAR(50), transaction_id VARCHAR(50), transaction_timestamp DATETIME, thread_id VARCHAR(50), spid INTEGER, connection_ VARCHAR(50));

CREATE TABLE cql_table_1(cql_table_1_pk INTEGER PRIMARY KEY AUTOINCREMENT, varchar_column VARCHAR(30));
CREATE TABLE cql_table_2(cql_table_2_pk INTEGER PRIMARY KEY AUTOINCREMENT, varchar_column VARCHAR(30), decimal_column DECIMAL(30,10));

*/
