/*  Part of SWI-Prolog

    Author:        Jan van der Steen, Matt Lilley and Jan Wielemaker,
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2015, SWI-Prolog Foundation
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(ssl,
	  [ load_certificate/2,         % +Stream, -Certificate
            load_private_key/3,         % +Stream, +Password, -Key
            load_public_key/2,          % +Stream, -Key
            load_crl/2,                 % +Stream, -Crl
	    system_root_certificates/1, % -List
	    cert_accept_any/5,		% +SSL, +ProblemCertificate,
					% +AllCertificates, +FirstCertificate,
					% +Error
            rsa_private_decrypt/3,      % +Key, +Ciphertext, -Plaintext
            rsa_private_encrypt/3,      % +Key, +Plaintext, -Ciphertext
            rsa_public_decrypt/3,       % +Key, +Ciphertext, -Plaintext
            rsa_public_encrypt/3,       % +Key, +Plaintext, -Ciphertext
            rsa_private_decrypt/4,      % +Key, +Ciphertext, -Plaintext, +Enc
            rsa_private_encrypt/4,      % +Key, +Plaintext, -Ciphertext, +Enc
            rsa_public_decrypt/4,       % +Key, +Ciphertext, -Plaintext, +Enc
            rsa_public_encrypt/4,       % +Key, +Plaintext, -Ciphertext, +Enc
            ssl_context/3,		% +Role, -Config, :Options
            ssl_init/3,                 % -Config, +Role, :Options
            ssl_accept/3,               % +Config, -Socket, -Peer
            ssl_open/3,                 % +Config, -Read, -Write
            ssl_open/4,                 % +Config, +Socket, -Read, -Write
            ssl_negotiate/5,            % +Config, +PlainRead, +PlainWrite,
					%          -SSLRead,   -SSLWrite
	    ssl_peer_certificate/2,	% +Stream, -Certificate
            ssl_session/2,              % +Stream, -Session
	    ssl_exit/1			% +Config
	  ]).
:- use_module(library(socket)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(debug)).

:- use_foreign_library(foreign(ssl4pl)).

:- meta_predicate
	ssl_init(-, +, :),
	ssl_context(+, -, :).

:- predicate_options(ssl_context/3, 3,
		     [ host(atom),
		       port(integer),
		       certificate_file(atom),
		       key_file(atom),
		       password(any),
		       pem_password_hook(callable),
		       cacert_file(any),
                       crl(any),
                       require_crl(boolean),
		       cert_verify_hook(callable),
		       cert(boolean),
		       peer_cert(boolean),
		       close_parent(boolean)
		     ]).
:- predicate_options(ssl_init/3, 3, [pass_to(ssl_context/3, 3)]).

/** <module> Secure Socket Layer (SSL) library

An SSL server and client can be built with the (abstracted)
predicate calls from the table below.  The `tcp_` predicates
are provided by library(socket).  The predicate ssl_context/3
defines properties of the SSL connection, while ssl_negotiate/5
establishes the SSL connection based on the wire streams created
by the TCP predicates and the context.

	| *The SSL Server*	| *The SSL Client*	|
	| ssl_context/3		| ssl_context/3		|
	| tcp_socket/1		| tcp_socket/1		|
	| tcp_accept/3		| tcp_connect/2		|
	| tcp_open_socket/3	| tcp_open_socket/3	|
	| ssl_negotiate/5	| ssl_negotiate/5	|

The library is abstracted to communication over streams, and is not
reliant on those streams being directly attached to sockets. The `tcp_`
calls here are simply the most common way to use the library. Other
two-way communication channels such as (named), pipes can just as
easily be used.

@see library(socket), library(http/http_open)
*/

%%	ssl_context(+Role, -SSL, :Options) is det.
%
%	Create an SSL context. The defines several properties of the SSL
%	connection such as  involved  keys,   preferred  encryption  and
%	passwords. After establishing a context,   an SSL connection can
%	be negotiated using ssl_negotiate/5, turning two arbitrary plain
%	Prolog streams into encrypted streams.  This predicate processes
%	the options below.
%
%	  * certificate_file(+FileName)
%	  Specify where the certificate file can be found. This can
%	  be the same as the key_file(+FileName) option.  A certificate
%	  file is obligatory for a server and may be provided for a
%	  client if the server demands the client to identify itself
%	  with a client certificate using the peer_cert(true) option. If
%	  a certificate is provided, it is always necessary to provide a
%	  matching \jargon{private key} using the key_file(+FileName)
%	  option.
%	  * key_file(+FileName)
%	  Specify where the private key that matches the certificate can
%	  be found.  If the key is encrypted with a password, this must
%	  be supplied using the password(+Text) or
%	  pem_password_hook(:PredicateName) option.
%	  * password(+Text)
%	  Specify the password the private key is protected with (if
%	  any). If you do not want to store the password you can also
%	  specify an application defined handler to return the password
%	  (see next option).  Text is either an atom or string.  Using
%	  a string is preferred as strings are volatile and local
%	  resources.
%	  * pem_password_hook(:PredicateName)
%	  In case a password is required to access the private key the
%	  supplied predicate will be called to fetch it. The predicate
%	  is called as call(PredicateName, Password) and typically
%	  unifies `Password` with a _string_ containing the password.
%	  * require_crl(+Boolean)
%         If true (default is false), then all certificates will be
%         considered invalid unless they can be verified as not being
%         revoked. You can do this explicity by passing a list of CRL
%         filenames via the crl/1 option, or by doing it yourself in
%         the cert_verify_hook. If you specify require_crl(true) and
%         provide neither of these options, verification will necessarily
%         fail
%	  * crl(+ListOfFileNames)
%         Provide a list of filenames of PEM-encoded CRLs that will be
%         given to the context to attempt to establish that a chain of
%         certificates is not revoked. You must also set require_crl(true)
%         if you want CRLs to actually be checked by OpenSSL.
%	  * cacert_file(+FileName)
%	  Specify a file containing certificate keys of _trusted_
%	  certificates. The peer is trusted if its certificate is
%	  signed (ultimately) by one of the provided certificates. Using
%	  the FileName `system(root_certificates)` uses a list of
%	  trusted root certificates as provided by the OS. See
%	  system_root_certificates/1 for details.
%
%	  Additional verification of the peer certificate as well as
%	  accepting certificates that are not trusted by the given set
%	  can be realised using the hook
%	  cert_verify_hook(PredicateName).
%	  * cert_verify_hook(:PredicateName)
%	  The predicate ssl_negotiate/5 calls PredicateName as follows:
%
%	    ==
%	    call(PredicateName, +SSL,
%		 +ProblemCertificate, +AllCertificates, +FirstCertificate,
%		 +Error)
%	    ==
%
%	  In case the certificate was verified by one of the provided
%	  certifications from the `cacert_file` option, Error is unified
%	  with the atom `verified`. Otherwise it contains the error
%	  string passed from OpenSSL. Access will be granted iff the
%	  predicate succeeds. See load_certificate/2 for a description
%	  of the certificate terms. See cert_accept_any/5 for a dummy
%	  implementation that accepts any certificate.
%	  * cert(+Boolean)
%	  Trigger the sending of our certificate specified by
%	  certificate_file(FileName).  Sending is automatic for the
%	  server role and implied if both a certificate and key are
%	  supplied for clients, making this option obsolete.
%	  * peer_cert(+Boolean)
%	  Trigger the request of our peer's certificate while
%	  establishing the SSL layer. This option is automatically
%	  turned on in a client SSL socket.  It can be used in a server
%	  to ask the client to identify itself using an SSL certificate.
%	  * close_parent(+Boolean)
%	  If `true`, close the raw streams if the SSL streams are closed.
%	  Default is `false`.
%	  * disable_ssl_methods(+List)
%	  A list of methods to disable. Unsupported methods will be
%	  ignored. Methods include `sslv2`, `sslv2`, `sslv23`,
%	  `tlsv1`, `tlsv1_1` and `tlsv1_2`.
%	  * ssl_method(+Method)
%	  Specify the explicit Method to use when negotiating. For
%	  allowed values, see the list for `disable_ssl_methods` above.
%
%	@arg Role is one of `server` or `client` and denotes whether the
%	SSL  instance  will  have  a  server   or  client  role  in  the
%	established connection.
%	@arg SSL is a SWI-Prolog _blob_ of type `ssl_context`, i.e., the
%	type-test for an SSL context is `blob(SSL, ssl_context)`.

ssl_context(Role, SSL, Module:Options) :-
	select_option(ssl_method(Method), Options, O1, sslv23),
	'_ssl_context'(Role, SSL, Module:O1, Method).

%%	ssl_negotiate(+SSL,
%%		      +PlainRead, +PlainWrite,
%%		      -SSLRead, -SSLWrite) is det.
%
%	Once a connection is established and a read/write stream pair is
%	available, (PlainRead and PlainWrite),  this   predicate  can be
%	called to negotiate an SSL  session   over  the  streams. If the
%	negotiation is successful, SSLRead and SSLWrite are returned.
%
%	@error ssl_error(Code, LibName, FuncName, Reason) is raised
%	if the negotiation fails. The streams PlainRead and PlainWrite
%	are *not* closed, but an unknown amount of data may have been
%	read and written.

%%	ssl_peer_certificate(+Stream, -Certificate) is semidet.
%
%	True if the peer certificate  is   provided  (this is always the
%	case for a client connection) and   Certificate unifies with the
%	peer certificate. The example below  uses   this  to  obtain the
%	_Common Name_ of the peer  after   establishing  an https client
%	connection:
%
%	  ==
%	    http_open(HTTPS_url, In, []),
%	    ssl_peer_certificate(In, Cert),
%	    memberchk(subject(Subject), Cert),
%	    memberchk('CN' = CommonName), Subject)
%	  ==

%%	ssl_session(+Stream, -Session) is det.
%
%	Retrieves (debugging) properties from the SSL context associated
%	with Stream. If Stream  is  not   an  SSL  stream, the predicate
%	raises  a  domain  error.  Session  is  a  list  of  properties,
%	containing the members described below.   Except  for `Version`,
%	all information are byte arrays that   are represented as Prolog
%	strings holding characters in the range 0..255.
%
%	  * ssl_version(Version)
%	  The negotiated version of the session as an integer.
%	  * session_key(Key)
%	  The key material used in SSLv2 connections (if present).
%	  * master_key(Key)
%	  The key material comprising the master secret. This is
%	  generated from the server_random, client_random and pre-master
%	  key.
%	  * client_random(Random)
%	  The random data selected by the client during handshaking.
%	  * server_random(Random)
%	  The random data selected by the server during handshaking.
%	  * session_id(SessionId)
%	  The SSLv3 session ID. Note that if ECDHE is being used (which
%	  is the default for newer versions of OpenSSL), this data will
%	  not actually be sent to the server.

%%	load_certificate(+Stream, -Certificate) is det.
%
%	Loads a certificate from a PEM- or DER-encoded stream, returning
%	a term which will unify with   the same certificate if presented
%	in cert_verify_hook. A certificate  is   a  list  containing the
%	following terms: issuer_name/1, hash/1,  signature/1, version/1,
%	notbefore/1,  notafter/1,  serial/1,   subject/1    and   key/1.
%	subject/1  and  issuer_name  are  both    lists   of  =/2  terms
%	representing the name.
%
%	Note that the OpenSSL `CA.pl`  utility creates certificates that
%	have a human readable textual representation in front of the PEM
%	representation. You can  use  the  following   to  skip  to  the
%	certificate if you know it is a PEM certificate:
%
%	  ==
%	  skip_to_pem_cert(In) :-
%		repeat,
%		(   peek_char(In, '-')
%		->  !
%		;   skip(In, 0'\n),
%		    at_end_of_stream(In), !
%		).
%	  ==

%%	load_crl(+Stream, -CRL) is det.
%
%	Loads a CRL from a PEM- or  DER-encoded stream, returning a term
%	containing  terms  hash/1,   signature/1,    issuer_name/1   and
%	revocations/1,  which  is  a  list   of  revoked/2  terms.  Each
%	revoked/2 term is of the form revoked(+Serial, DateOfRevocation)

%%	system_root_certificates(-List) is det.
%
%	List is a list of trusted root   certificates as provided by the
%	OS. This is the list used by ssl_context/3 when using the option
%	`system(root_certificates)`.  The list is obtained using an OS
%	specific process.  The current implementation is as follows:
%
%	    - On Windows, CertOpenSystemStore() is used to import
%	      the `"ROOT"` certificates from the OS.
%	    - On MacOSX, the trusted keys are loaded from the
%	      _SystemRootCertificates_ key chain.  The Apple API
%	      for this requires the SSL interface to be compiled
%	      with an XCode compiler, i.e., *not* with native gcc.
%	    - Otherwise, certificates are loaded from a file defined
%	      by the Prolog flag `system_cacert_filename`.  The initial
%	      value of this flag is operating system dependent.  For
%	      security reasons, the flag can only be set prior to using
%	      the SSL library.  For example:
%
%	        ==
%	        :- use_module(library(ssl)).
%	        :- set_prolog_flag(system_cacert_filename,
%				   '/home/jan/ssl/ca-bundle.crt').
%	        ==

%%      load_private_key(+Stream, +Password, -PrivateKey) is det.
%
%	Load a private key PrivateKey  from   the  given  stream Stream,
%	using Password to decrypt the key if  it is encrypted. Note that
%	the  password  is  currently  only   supported  for  PEM  files.
%	DER-encoded keys which are password protected will not load. The
%	key must be an RSA key. EC, DH   and DSA keys are not supported,
%	and PrivateKey will be  bound  to   an  atom  (ec_key, dh_key or
%	dsa_key) if you try and load   such  a key. Otherwise PrivateKey
%	will be unified with privtate_key(KeyTerm)   where  KeyTerm is a
%	rsa/8 term representing an RSA key.

%%      load_public_key(+Stream, -PublicKey) is det.
%
%	Load a public key  PublicKey  from   the  given  stream  Stream.
%	Supports loading both DER- and PEM-encoded keys. The key must be
%	an RSA key. EC, DH and DSA keys are not supported, and PublicKey
%	will be bound to an atom (one   of ec_key, dh_key or dsa_key) if
%	you try and load such a key. Otherwise PublicKey will be unified
%	with  public_key(KeyTerm)  where  KeyTerm  is    an  rsa/8  term
%	representing an RSA key.

%%	rsa_private_decrypt(+PrivateKey, +CipherText, -PlainText) is det.
%%      rsa_private_encrypt(+PrivateKey, +PlainText, -CipherText) is det.
%%      rsa_public_decrypt(+PublicKey, +CipherText, -PlainText) is det.
%%      rsa_public_encrypt(+PublicKey, +PlainText, -CipherText) is det.
%%	rsa_private_decrypt(+PrivateKey, +CipherText, -PlainText, +Enc) is det.
%%      rsa_private_encrypt(+PrivateKey, +PlainText, -CipherText, +Enc) is det.
%%      rsa_public_decrypt(+PublicKey, +CipherText, -PlainText, +Enc) is det.
%%      rsa_public_encrypt(+PublicKey, +PlainText, -CipherText, +Enc) is det.
%
%	RSA Public key encryption and   decryption  primitives. A string
%	can be safely communicated by first   encrypting it and have the
%	peer decrypt it with the matching  key and predicate. The length
%	of the string is limited by  the   key  length.  Text is encoded
%	using encoding Enc, which is one   of  `octet`, `text` or `utf8`
%	(default).
%
%	@see load_private_key/3, load_public_key/2 can be use to load
%	keys from a file.  The predicate load_certificate/2 can be used
%	to obtain the public key from a certificate.
%
%	@error ssl_error(Code, LibName, FuncName, Reason)   is raised if
%	there is an error, e.g., if the text is too long for the key.

rsa_private_decrypt(PrivateKey, CipherText, PlainText) :-
	rsa_private_decrypt(PrivateKey, CipherText, PlainText, utf8).

rsa_private_encrypt(PrivateKey, PlainText, CipherText) :-
	rsa_private_encrypt(PrivateKey, PlainText, CipherText, utf8).

rsa_public_decrypt(PublicKey, CipherText, PlainText) :-
	rsa_public_decrypt(PublicKey, CipherText, PlainText, utf8).

rsa_public_encrypt(PublicKey, PlainText, CipherText) :-
	rsa_public_encrypt(PublicKey, PlainText, CipherText, utf8).

/*
  These predicates are here to support backward compatibility with the previous
  incarnation of the SSL library. No changes should be required for legacy code.
*/

%%	ssl_init(-SSL, +Role, +Options) is det.
%
%	Create an SSL context.  Similar to ssl_context/3.
%
%	@deprecated   New   code   should     use    ssl_context/3   and
%	ssl_negotiate/5 to realise an SSL connection.

ssl_init(SSL, Role, Options) :-
	must_be(oneof([client,server]), Role),
	ssl_init2(Role, SSL, Options).

ssl_init2(server, SSL, Options) :-
	Options = _:Options1,
	need_option(port(Port), Options1),
        tcp_socket(Socket),
	assertion(Socket = '$socket'(_)),	% may change
	tcp_setopt(Socket, reuseaddr),
        tcp_bind(Socket, Port),
        tcp_listen(Socket, 5),
        catch(ssl_context(server, SSL, Options),
              Exception,
              ( tcp_close_socket(Socket),
                throw(Exception))),
        Socket = '$socket'(S),
        ssl_put_socket(SSL, S).
ssl_init2(client, SSL, Options) :-
	Options = _:Options1,
        need_option(port(Port), Options1),
        need_option(host(Host), Options1),
        tcp_socket(Socket),
	assertion(Socket = '$socket'(_)),	% may change
	tcp_setopt(Socket, reuseaddr),
        tcp_connect(Socket, Host:Port),
        catch(ssl_context(client, SSL, Options),
              Exception,
              ( tcp_close_socket(Socket),
                throw(Exception))),
        Socket = '$socket'(S),
        ssl_put_socket(SSL, S).

need_option(Opt, Options) :-
	option(Opt, Options), !.
need_option(Opt, _) :-
	functor(Opt, Name, _),
	existence_error(option, Name).

%%	ssl_accept(+SSL, -Socket, -Peer) is det.
%
%	(Server) Blocks until a connection is made   to  the host on the
%	port specified by the  SSL  object.   Socket  and  Peer are then
%	returned.
%
%	@deprecated   New   code    should     use    tcp_accept/3   and
%	ssl_negotiate/5.

ssl_accept(SSL, Socket, Peer) :-
        ssl_get_socket(SSL, S),
        tcp_accept('$socket'(S), Socket, Peer).

%%	ssl_open(+SSL, -Read, -Write) is det.
%
%	(Client) Connect to the  host  and   port  specified  by the SSL
%	object, negotiate an SSL connection and   return  Read and Write
%	streams if successful.  It  calls   ssl_open/4  with  the socket
%	associated  to  the  SSL  instance.  See  ssl_open/4  for  error
%	handling.
%
%	@deprecated New code should use ssl_negotiate/5.

ssl_open(SSL, In, Out) :-
        ssl_get_socket(SSL, Socket),
	ssl_open(SSL, '$socket'(Socket), In, Out).

%%	ssl_open(+SSL, +Socket, -Read, -Write) is det.
%
%	Given the Socket  returned  from   ssl_accept/3,  negotiate  the
%	connection on the accepted socket  and   return  Read  and Write
%	streams if successful. If ssl_negotiate/5   raises an exception,
%	the Socket is closed and the exception is re-thrown.
%
%	@deprecated New code should use ssl_negotiate/5.

ssl_open(SSL, Socket, In, Out):-
        tcp_open_socket(Socket, Read, Write),
        catch(ssl_negotiate(SSL, Read, Write, In, Out), E,
	      ssl_open_failed(Read, Write, E)).

ssl_open_failed(Read, Write, Error) :-
	close(Read, [force(true)]),
	close(Write, [force(true)]),
	throw(Error).

%%	ssl_exit(+SSL)
%
%	Free an SSL context. SSL contexts   are  reclaimed by the Prolog
%	(atom) garbage collector. Calling ssl_exit/1   is  needed if the
%	deprecated  ssl_init/3  interface  is  used    rather  than  the
%	ssl_context/3 based interface to reclaim the associated socket.

ssl_exit(SSL) :-
	(   ssl_get_socket(SSL, Socket),
	    Socket \== -1
	->  tcp_close_socket('$socket'(Socket))
	;   true
	),
	'_ssl_exit'(Socket).


%%	cert_accept_any(+SSL,
%%			+ProblemCertificate, +AllCertificates, +FirstCertificate,
%%			+Error) is det.
%
%	Implementation  for  the  hook   `cert_verify_hook(:Hook)`  that
%	accepts _any_ certificate. This is   intended for http_open/3 if
%	no certificate verification is desired as illustrated below.
%
%	  ==
%	    http_open('https:/...', In,
%	              [ cert_verify_hook(cert_accept_any)
%	              ])
%	  ==

cert_accept_any(_SSL,
		_ProblemCertificate, _AllCertificates, _FirstCertificate,
		_Error).


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile
	prolog:error_message//1.

prolog:error_message(ssl_error(ID, _Library, Function, Reason)) -->
	[ 'SSL(~w) ~w: ~w'-[ID, Function, Reason] ].
