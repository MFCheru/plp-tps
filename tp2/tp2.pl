% Definiciones de operadores.
:- op(900,xfy, [ + ]).
:- op(800,xfy, [ * ]).

% Implementación de los predicados.

% Consideramos que para que un proceso este bien tipado sus elemento deben ser atomicos, de ahi la condicion de atom(U) del proceso U*P, en algunas reglas.
% Notar que la razon por la que usamos list\_to\_set, es para convertir la lista en un "conjunto", y de esta forma evitar la repeticion elementos en la salida.

%1) acciones(+Proceso, -Acciones).
% Recordamos que tau es una accion interna, y no una accion "tradicinal", por ende segun la definicion del enunciado no debe pertencer al conjunto de acciones de esta consulta.

acciones(0, []).
acciones(tau*P, As) :- acciones(P, As), !.
acciones(U*P, As) :- atom(U), acciones(P, Ap), append([U], Ap, Atot), list_to_set(Atot, As).
acciones(P+Q, As) :- acciones(P, Ap), acciones(Q, Aq), append(Ap, Aq, Atot), list_to_set(Atot, As).

%2) reduce(+Proceso1, ?Accion, ?Proceso2).

reduce(U*P, U, P) :- atom(U).
reduce(P+Q, U, R) :- reduce(P, U, R); reduce(Q, U, R).

%3) reduceLista(+Proceso1, ?Cadena, ?Proceso2).
% Notar que pedimos que para que la consulta sea valida, tau no debe pertenecer a la cadena.

reduceLista(P, [], P).
reduceLista(tau*P, [], P).
reduceLista(P, [X|Xs], S) :- reduce(P, X, R), X \= tau, reduceLista(R, Xs, S).

%4) trazas(+Proceso, -Cadenas).

trazas(P, C) :- findall(X, reduceLista(P, X, S), L), list_to_set(L, C).

%5) residuo(+Procesos1, +Cadena, -Procesos2).

residuo(P, [], [P]).
residuo(P+Q, C, R) :- residuo(P, C, Rp), residuo(Q, C, Rq), append(Rp, Rq, L), list_to_set(L, R).
residuo(U*P, C, R) :- findall( X, reduceLista(U*P, C, X), L), list_to_set(L, R).
residuo([], C, []).
residuo([P|Ps], C, R) :- findall( X, reduceLista(P, C, X), Rp), residuo(Ps, C, RPs), append(Rp, RPs, Rtot), list_to_set(Rtot, R).

%6) must(+Procesos, +Cadena).
% Si el proceso empieza con tau, consultamos must sobre el proceso sin ese tau.
% Si el proceso tiene una bifurcacion, consultamos must de la primera rama, si se satisface la consulta, ya significa que habia en la cadena un elemento que podia reducir sobre el proceso, si no
% revisamos la otra rama. Notar que al revisar si puedo reducir al proceso con algun elemento de la cadena, dejo de revisar apenas encuentro al primero que reduzca.

must(tau*P, C) :- must(P, C), !.
must(P+Q, C) :- must(P, C), ! ; must(Q, C).
must(U*P, [X|Xs]) :- reduce(U*P, X, P), ! ; must(U*P, Xs).
must([], C).
must([P|Ps], C) :- must(P, C), must(Ps, C).

%7) puedeReemplazarA(+Proceso1, +Proceso2).
% Para este ejercicio consideramos la negacion del predicado dado en el enunciado (convirtiendo previamente la implicacion), al que luego negamos con not.
% De esta forma la consulta sera satisfecha si no existe ninguna traza S y accion L de P o Q, tal que se satisface (P after S) must L y (Q after S), pero no (Q after S) must L.
% Para esto usamos la tecnica de generate \& test, generando en el primer reglon las trazas de P y Q, y luego tomando S como elemento de la union entre ambas; analogamente hacemos lo mismo
% para L, generamos en el segundo renglon las acciones de P y Q, y tomamos a L como elemento de la union entre ambas. Por ultimo en el tercer renglon generamos los procesos resultantes de reducir
% a P y Q con S, y testeamos los must con [L].
 
puedeReemplazarA(P, Q) :- not( (trazas(P, Tp), trazas(Q, Tq), append(Tp, Tq, Tu), list_to_set(Tu, T), member(S, T),
				acciones(P, Ap), acciones(Q, Aq), append(Ap, Aq, Au), list_to_set(Au, A), member(L, A),
				reduceLista(P, S, Ps), must(Ps, [L]), reduceLista(Q, S, Qs), not(must(Qs, [L]))) ).


%8) equivalentes(+Proceso1, +Proceso2).
% Consideramos que dos procesos P y Q son equivalente sii P puede reemplazar a Q, y viceversa.

equivalentes(P, Q) :- puedeReemplazarA(P, Q), puedeReemplazarA(Q, P).

% Tests (van un par de ejemplos, agreguen los suyos).

test(0) :- not((acciones(0, L), member(_,L))).

test(1) :- reduceLista(0,[],0).

test(2) :- not(puedeReemplazarA(moneda * (te * 0 + cafe * 0), moneda * te * 0 + moneda * cafe * 0)).

test(3) :- puedeReemplazarA(tau*a*0, a*0).

test(4) :- equivalentes(a*b*(c*0+d*0), a*b*(d*tau*0+c*0)).

test(5) :- not(equivalentes(a*b*0, b*a*0)).

test(6) :- not( (acciones( tau*0 + tau*0, A), member(tau, A)) ).

test(7) :- not( must(0, []) ).

test(8) :- trazas(0, [[]]).

test(9) :- residuo([(a*b*(c*a*0 + d*c*0) + a*b*c*0), (a*b*0)], [a,b], R), permutation([c*a*0+d*c*0, c*0, 0], R), !. 

test(10) :- residuo(tau*0, [], [tau*0]).

test(11) :- must(tau*(tau*a*0 + tau*b*0), [a]).

test(12) :- not( reduce(tau*a*0, [a], 0) ).

test(13) :- must([(a*0+b*c*0),(b*0+a*c*0)],[a]).

test(14) :- not( reduce(tau*a*tau*0, a) ).

test(15) :- reduceLista(tau*0, [], 0), !.

tests :- forall(between(0, 5, N), test(N)). %Actualizar la cantidad total de tests para contemplar los que agreguen ustedes.
