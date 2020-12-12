:- use_module(library(csv)).

%%% Graph
% Predicati definiti dinamicamente
:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.

%%% Fatti
graph(grafo).
vertex(grafo, a).
vertex(grafo, b).
vertex(grafo, c).
vertex(grafo, d).
vertex(grafo, e).
vertex(grafo, f).
vertex(grafo, g).
vertex(grafo, h).
vertex(grafo, i).
arc(grafo, a, b, 4).
arc(grafo, a, h, 8).
arc(grafo, b, c, 8).
arc(grafo, b, h, 11).
arc(grafo, c, d, 7).
arc(grafo, c, f, 4).
arc(grafo, c, i, 2).
arc(grafo, d, e, 9).
arc(grafo, d, f, 14).
arc(grafo, e, f, 10).
arc(grafo, f, g, 2).
arc(grafo, g, h, 1).
arc(grafo, g, i, 6).
arc(grafo, h, i, 7).

% Questo predicato inserisce un nuovo grafo nella base-dati Prolog.
new_graph(G) :- 
    graph(G), 
    !.

new_graph(G) :- 
    assert(graph(G)), 
    !.

% Rimuove tutto il grafo (vertici e archi inclusi) dalla base-dati Prolog.
delete_graph(G) :-
    retract(graph(G)),
    retractall(vertex(G, _)),
    retractall(arc(G, _, _, _)).

% Aggiunge il vertice V nella base-dati Prolog.
new_vertex(G, V) :-
    graph(G),
    assert(vertex(G, V)).

% Questo predicato è vero quando Vs è una lista contenente tutti i vertici di G.
graph_vertices(G, Vs) :-
    graph(G),
    findall(V, vertex(G, V), Vs).

% Questo predicato stampa alla console dell’interprete Prolog una lista dei vertici del grafo G.
list_vertices(G) :-
    graph(G),
    listing(vertex(G, _)).

% Aggiunge un arco del grafo G alla base dati Prolog.
%% TODO: Cosa fare se l'arco è già presente?
new_arc(G, U, V, Weight) :-
    graph(G),
    vertex(G, U),
    vertex(G, V),
    assert(arc(G, U, V, Weight)).

new_arc(G, U, V) :- 
    new_arc(G, U, V, 1).

% Questo predicato è vero quando Es è una lista di tutti gli archi presenti in G.
graph_arcs(G, Es) :-
    graph(G),
    findall(arc(G, U, V, W), arc(G, U, V, W), Es).

% Questo predicato è vero quando V è un vertice di G e Ns è una lista contenente gli archi che portano ai vertici N immediatamente raggiungibili da V.
vertex_neighbors(G, V, Ns) :-
    vertex(G, V),
    findall(arc(G, V, N, W), arc(G, V, N, W), Ns).

% Questo predicato è vero quando V è un vertice di G e Vs è una lista contenente i vertici ad esso adiacenti.
adjs(G, V, Vs) :- 
    vertex(G, V),
    findall(vertex(G, U), arc(G, V, U, _), VOs),
    findall(vertex(G, U), arc(G, U, V, _), VIs),
    merge(VOs, VIs, Vs).

merge([], Ys, Ys).

merge([X | Xs], Ys, [X | Zs]) :-
    merge(Xs, Ys, Zs).

% Questo predicato stampa alla console dell’interprete Prolog una lista degli archi del grafo G.
list_arcs(G) :-
    graph(G),
    listing(arc(G, _, _, _)).

% Questo predicato stampa alla console dell’interprete Prolog una lista dei vertici e degli archi del grafo G.
list_graph(G) :-
    list_vertices(G),
    list_arcs(G).

% Questo predicato legge un “grafo” G, da un file FileName e lo inserisce nel data base di Prolog.
%% TODO: Nella lettura sarebbe meglio asserire anche i vertici
read_graph(G, FileName) :-
    csv_read_file(FileName, Rows, [functor(arc), arity(3), separator(0'\t)]),
    maplist(assert_results(G), Rows).

assert_results(G, Rows) :-
    Rows =.. [R | Rs],
    Arc =.. [R, G | Rs],
    assert(Arc).

% Questo predicato è vero quando G viene scritto sul file FileName secondo il valore dell’argomento Type. Type può essere graph o edges.
%% TODO: Introdurre determinismo tra i /3
write_graph(G, FileName) :-
   write_graph(G, FileName, graph).

write_graph(G, FileName, graph) :-
    graph_arcs(G, Es),
    maplist(prepare_output, Es, Rows),
    csv_write_file(FileName, Rows, [separator(0'\t)]).
 
write_graph(G, FileName, edges) :-
    maplist(prepare_output, G, Rows),
    csv_write_file(FileName, Rows, [separator(0'\t)]).

prepare_output(Arc, Row) :-
    Arc =.. [P, _ | Gs],
    Row =.. [P | Gs].

%%% MinHeap
% Predicati definiti dinamicamente
:- dynamic heap/2.
:- dynamic heap_entry/4.

% Questo predicato inserisce un nuovo heap nella base-dati Prolog.
new_heap(H) :-
    heap(H, _),
    !.

new_heap(H) :-
    assert(heap(H, 0)),
    !.

% Rimuove tutto lo heap dalla base-dati Prolog.
delete_heap(H) :-
    retract(heap(H, _)),
    retractall(heap_entry(H, _, _, _)).

% Questo predicato è vero quando S è la dimensione corrente dello heap.
heap_has_size(H, S) :-
    heap(H, S).

% Questo predicato è vero quando lo heap H non contiene elementi.
heap_empty(H) :-
    heap(H, 0).

% Questo predicato è vero quando lo heap H contiene almeno un elemento
heap_not_empty(H) :-
    heap(H, S),
    S > 0.

% Il predicato è vero quando l’elemento dello heap H con chiave minima K è V.
heap_head(H, K, V) :-
    heap_not_empty(H),
    heap_entry(H, 1, K, V).

% Il predicato è vero quando l’elemento Vèinserito nelloheap Hcon chiave K.
heap_insert(H, K, V) :-
    heap(H, S),
    retract(heap(H, S)),
    S1 is S + 1,
    assert(heap(H, S1)),
    assert(heap_entry(H, S1, K, V)),
    heapify_insert(H, S1).

heapify_insert(_, I) :-
    I =< 1,
    !.

heapify_insert(_, I) :-
    P is floor(I / 2),
    heap_entry(H, I, K, _),
    heap_entry(H, P, PK, _),
    K >= PK,
    !.

heapify_insert(H, I) :-
    P is floor(I / 2),
    heap_entry(H, I, K, V),
    heap_entry(H, P, PK, PV),
    K < PK,
    !,
    retract(heap_entry(H, I, K, V)),
    retract(heap_entry(H, P, PK, PV)),
    assert(heap_entry(H, P, K, V)),
    assert(heap_entry(H, I, PK, PV)),
    heapify_insert(H, P).

% Il predicato è vero quando la coppia K, V con K minima, è rimossa dallo heap H.
% TODO: In certe situazioni sembra dare più di una scelta, che non dovrebbe esserci e che comunque fallisce sempre (teoricamente è già stato fixato)
% TODO: L'extract dovrebbe permettere di togliere solo l'head
heap_extract(H, K, V) :-
    heap_entry(H, _, K, V),
    heap_has_size(H, S),
    compare(=, S, 1),
    !,
    retract(heap_entry(H, _, K, V)),
    retract(heap(H, S)),
    assert(heap(H, 0)).

heap_extract(H, K, V) :-
    heap_entry(H, I, K, V),
    heap_has_size(H, S),
    S > 1,
    compare(=, S, I),
    !,
    retract(heap_entry(H, I, K, V)),
    retract(heap(H, S)),
    S1 is S - 1,
    assert(heap(H, S1)).

heap_extract(H, K, V) :-
    heap_entry(H, I, K, V),
    heap_has_size(H, S),
    S > 1,
    S > I,
    !,
    retract(heap_entry(H, S, SK, SV)),
    retract(heap_entry(H, I, K, V)),
    assert(heap_entry(H, I, SK, SV)),
    retract(heap(H, S)),
    S1 is S - 1,
    assert(heap(H, S1)),
    heapify(H, I).

heapify(H, I) :- 
    L is I * 2,
    R is I * 2 + 1,
    heap_has_size(H, S),
    L > S,
    R > S,
    !. 

heapify(H, I) :-
    L is I * 2,
    R is I * 2 + 1,
    smallest(H, I, L, Stemp),
    smallest(H, Stemp, R, Smallest),
    !,
    swap(H, I, Smallest).

smallest(H, X, Y, Smallest) :-
    heap_has_size(H, S),
    Y =< S,
    heap_entry(H, X, XK, _),
    heap_entry(H, Y, YK, _),
    YK =< XK,
    !,
    Smallest is Y.

smallest(H, X, Y, Smallest) :-
    heap_has_size(H, S),
    Y =< S,
    heap_entry(H, X, XK, _),
    heap_entry(H, Y, YK, _),
    YK > XK,
    !,
    Smallest is X.

smallest(H, X, Y, Smallest) :-
    heap_has_size(H, S),
    Y > S,
    !,
    Smallest is X.

swap(_, X, Y) :-
    compare(=, X, Y),
    !.

swap(H, X, Y) :-
    X \= Y,
    !,
    retract(heap_entry(H, Y, YK, YV)),
    retract(heap_entry(H, X, XK, XV)),
    assert(heap_entry(H, X, YK, YV)),
    assert(heap_entry(H, Y, XK, XV)),
    heapify(H, Y).

% Il predicato è vero quando la chiave OldKey (associata al valore V) è sostituita da NewKey.
% N.B.: Predicato da implementare solo se è necessario. Per adesso ne creo una bozza, eventualmente lo elimino.
% TODO: Dopo aver modificato l'elemento potrebbe essere necessario sistemare lo heap
modify_key(H, NewKey, OldKey, V) :-
    heap(H, _),
    heap_entry(H, S, OldKey, V),
    retract(heap_entry(H, S, OldKey, V)),
    assert(heap_entry(H, S, NewKey, V)).

% Il predicato stampa sulla console Prolog lo stato interno dello heap
list_heap(H) :-
    heap(H, _),
    listing(heap(H, _)),
    listing(heap_entry(H, _, _, _)).

%%% Minimum Spanning Trees
% Predicati definiti dinamicamente
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.

% mst_prim/2
mst_prim(G, Source) :-
    vertex(G, Source),
    new_heap(G),
    heap_insert(G, 0, arc(G, Source, nil, inf)),
    build_mst(G, Source).

build_mst(G, H) :-
    adjs(G, H, As),
    mst_queue_from_list(G, H, As),
    mst_add_vertex(G).

mst_queue_from_list(_, _, []) :-
    !.

mst_queue_from_list(G, H, [A | As]) :-
    A =.. [_, _, V],
    vertex_key(G, V, W1),
    arc(G, H, V, W2),
    W1 =< W2,
    !,
    mst_queue_from_list(G, H, As).

mst_queue_from_list(G, H, [A | As]) :-
    A =.. [_, _, V],
    vertex_key(G, V, W1),
    arc(G, V, H, W2),
    W1 =< W2,
    !,
    mst_queue_from_list(G, H, As).

%% TODO: Credo ci sia il rischio che un elemento possa essere aggiunto all'heap più volte
%%       EG: X non è nell'albero ma è già presente nell'heap con un altro arco o addiritutta con lo stesso arco
%%           Non dovrebbe causare grossi problemi, se non nei tempi di esecuzione
mst_queue_from_list(G, H, [A | As]) :-
    A =.. [_, _, V],
    vertex_key(G, V, W1),
    arc(G, H, V, W2),
    W2 < W1,
    !,
    heap_insert(G, W2, arc(G, H, V, W2)),
    mst_queue_from_list(G, H, As).

mst_queue_from_list(G, H, [A | As]) :-
    A =.. [_, _, V],
    vertex_key(G, V, W1),
    arc(G, V, H, W2),
    W2 < W1,
    !,
    heap_insert(G, W2, arc(G, V, H, W2)),
    mst_queue_from_list(G, H, As).

mst_queue_from_list(G, H, [A | As]) :-
    A =.. [_, _, V],
    not(vertex_key(G, V, _)),
    arc(G, H, V, W),
    !,
    heap_insert(G, W, arc(G, V, H, W)),
    mst_queue_from_list(G, H, As).

mst_queue_from_list(G, H, [A | As]) :-
    A =.. [_, _, V],
    not(vertex_key(G, V, _)),
    arc(G, V, H, W),
    !,
    heap_insert(G, W, arc(G, V, H, W)),
    mst_queue_from_list(G, H, As).

mst_add_vertex(G) :-
    heap_empty(G),
    !.

mst_add_vertex(G) :-
    heap_not_empty(G),
    heap_head(G, K, V),
    V =.. [_, _, A, _, _],
    vertex_key(G, A, _),
    !,
    heap_extract(G, K, V),
    mst_add_vertex(G).

mst_add_vertex(G) :-
    heap_not_empty(G),
    heap_head(G, K, V),
    V =.. [_, _, A, H, _],
    not(vertex_key(G, A, _)),
    !,
    heap_extract(G, K, V),
    assert(vertex_key(G, A, K)),
    assert(vertex_previous(G, A, H)),
    build_mst(G, A). 

a :-
    listing(vertex_key(_, _, _)),
    listing(vertex_previous(_, _, _)).

% mst_get