/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007-2015, University of Amsterdam
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

:- module(http_ssl_plugin, []).
:- use_module(library(ssl)).
:- use_module(library(socket)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_header)).

/** <module> SSL plugin for HTTP libraries

This  module  can  be   loaded    next   to   library(thread_httpd)  and
library(http_open) to provide secure HTTP   (HTTPS)  services and client
access.

An example secure server using self-signed  certificates can be found in
the <plbase>/doc/packages/examples/ssl/https.pl, where <plbase>   is the
SWI-Prolog installation directory.
*/

:- multifile
	thread_httpd:make_socket_hook/3,
	thread_httpd:accept_hook/2,
	thread_httpd:open_client_hook/5,
        http:http_protocol_hook/5,
	http:open_options/2,
	http:http_connection_over_proxy/6.


		 /*******************************
		 *	    SERVER HOOKS	*
		 *******************************/

%%	thread_httpd:make_socket_hook(?Port, :OptionsIn, -OptionsOut)
%%								is semidet.
%
%	Hook into http_server/2 to create an   SSL  server if the option
%	ssl(SSLOptions) is provided.
%
%	@see thread_httpd:accept_hook/2 handles the corresponding accept

thread_httpd:make_socket_hook(Port, M:Options0, Options) :-
	memberchk(ssl(SSLOptions), Options0), !,
	make_socket(Port, Socket, Options0),
	ssl_context(server,
                    SSL,
                    M:[ port(Port),
                        close_parent(true)
                      | SSLOptions
                      ]),
	atom_concat('httpsd', Port, Queue),
	Options = [ queue(Queue),
                    tcp_socket(Socket),
		    ssl_instance(SSL)
		  | Options0
		  ].

make_socket(_Port, Socket, Options) :-
	option(tcp_socket(Socket), Options), !.
make_socket(Port, Socket, _Options) :-
	tcp_socket(Socket),
	tcp_setopt(Socket, reuseaddr),
	tcp_bind(Socket, Port),
	tcp_listen(Socket, 5).


%%	thread_httpd:accept_hook(:Goal, +Options) is semidet.
%
%	Implement the accept for HTTPS connections.

thread_httpd:accept_hook(Goal, Options) :-
	memberchk(ssl_instance(SSL), Options), !,
	memberchk(queue(Queue), Options),
        memberchk(tcp_socket(Socket), Options),
        tcp_accept(Socket, Client, Peer),
	debug(http(connection), 'New HTTPS connection from ~p', [Peer]),
	http_enough_workers(Queue, accept, Peer),
	thread_send_message(Queue, ssl_client(SSL, Client, Goal, Peer)).

thread_httpd:open_client_hook(ssl_client(SSL, Client, Goal, Peer),
			      Goal, In, Out,
			      [peer(Peer), protocol(https)]) :-
        tcp_open_socket(Client, Read, Write),
	catch(ssl_negotiate(SSL, Read, Write, In, Out),
	      E,
	      ssl_failed(Read, Write, E)).

ssl_failed(Read, Write, E) :-
	close(Write, [force(true)]),
	close(Read,  [force(true)]),
	throw(E).


		 /*******************************
		 *	   CLIENT HOOKS		*
		 *******************************/

%%	http:http_protocol_hook(+Scheme, +Parts, +PlainStreamPair,
%%				-StreamPair, +Options) is semidet.
%
%	Hook for http_open/3 to connect  to   an  HTTPS (SSL-based HTTP)
%	server.   This   plugin   also   passes   the   default   option
%	`cacert_file(system(root_certificates))` to ssl_context/3.

http:http_protocol_hook(https, Parts, PlainStreamPair, StreamPair, Options):-
	ssl_protocol_hook(Parts, PlainStreamPair, StreamPair, Options).

ssl_protocol_hook(Parts, PlainStreamPair, StreamPair, Options) :-
        memberchk(host(Host), Parts),
        option(port(Port), Parts, 443),
	ssl_context(client, SSL, [ host(Host),
                                   port(Port),
                                   close_parent(true)
				 | Options
				 ]),
        stream_pair(PlainStreamPair, PlainIn, PlainOut),
        catch(ssl_negotiate(SSL, PlainIn, PlainOut, In, Out),
              Exception,
              ( ssl_exit(SSL), throw(Exception)) ),
        stream_pair(StreamPair, In, Out).

%%	http:open_options(Parts, Options) is nondet.
%
%	Implementation of the multifile hook http:open_options/2 used by
%	library(http/http_open). By default, we use   the system trusted
%	root certificate database for validating an SSL certificate.

http:open_options(Parts, Options) :-
	memberchk(scheme(https), Parts),
	Options = [cacert_file(system(root_certificates))].

%%	http:http_connection_over_proxy(+Proxy, +Parts, +HostPort, -StreamPair,
%%					+OptionsIn, -OptionsOut)
%
%	Facilitate an HTTPS connection via a   proxy using HTTP CONNECT.
%	Note that most proxies will only  support this for connecting on
%	port 443

http:http_connection_over_proxy(proxy(ProxyHost, ProxyPort), Parts,
				Host:Port, StreamPair, Options, Options) :-
        memberchk(scheme(https), Parts), !,
        tcp_connect(ProxyHost:ProxyPort, StreamPair, [bypass_proxy(true)]),
        catch(negotiate_http_connect(StreamPair, Host:Port),
              Error,
              ( close(StreamPair, [force(true)]),
                throw(Error)
              )).

negotiate_http_connect(StreamPair, Address):-
        format(StreamPair, 'CONNECT ~w HTTP/1.1\r\n\r\n', [Address]),
        flush_output(StreamPair),
        http_read_reply_header(StreamPair, Header),
        memberchk(status(_, Status, Message), Header),
        (   Status == ok
	->  true
        ;   throw(error(proxy_rejection(Message), _))
        ).


