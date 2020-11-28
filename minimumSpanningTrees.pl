:- use_module(library(csv)).

%%% Graph
% Predicati definiti dinamicamente
:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.

%%% Fatti
graph(grafo).
vertex(grafo, 1).
vertex(grafo, 2).
vertex(grafo, 3).
vertex(grafo, 4).
arc(grafo, 1, 2, 1).
arc(grafo, 2, 3, 1).
arc(grafo, 1, 4, 1).

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
    findall(V, vertex(G, V), Vs).

% Questo predicato stampa alla console dell’interprete Prolog una lista dei vertici del grafo G.
list_vertices(G) :-
    graph(G),
    listing(vertex(G, _)).

% Aggiunge un arco del grafo G alla base dati Prolog.
new_arc(G, U, V, Weight) :-
    graph(G),
    vertex(G, U),
    vertex(G, V),
    assert(arc(G, U, V, Weight)).

new_arc(G, U, V) :- 
    new_arc(G, U, V, 1).

% Questo predicato è vero quando Es è una lista di tutti gli archi presenti in G.
graph_arcs(G, Es) :-
    findall(arc(G, U, V, W), arc(G, U, V, W), Es).

% Questo predicato è vero quando V è un vertice di G e Ns è una lista contenente gli archi che portano ai vertici N immediatamente raggiungibili da V.
vertex_neighbors(G, V, Ns) :-
    vertex(G, V),
    findall(arc(G, V, N, W), arc(G, V, N, W), Ns).

% Questo predicato è vero quando V è un vertice di G e Vs è una lista contenente i vertici ad esso adiacenti.
adjs(G, V, Vs) :- 
    vertex(G, V),
    findall(vertex(G, U), arc(G, V, U, _), Vs).

% Questo predicato stampa alla console dell’interprete Prolog una lista degli archi del grafo G.
list_arcs(G) :-
    graph(G),
    listing(arc(G, _, _, _)).

% Questo predicato stampa alla console dell’interprete Prolog una lista dei vertici e degli archi del grafo G.
list_graph(G) :-
    list_vertices(G),
    list_arcs(G).

% Questo predicato legge un “grafo” G, da un file FileName e lo inserisce nel data base di Prolog.
read_graph(G, FileName) :-
    csv_read_file(FileName, Rows, [functor(arc), arity(3), separator(0'\t)]),
    maplist(assert_results(G), Rows).

assert_results(G, Rows) :-
    Rows =.. [R | Rs],
    Arc =.. [R, G | Rs],
    assert(Arc).

% Questo predicato è vero quando G viene scritto sul file FileName secondo il valore dell’argomento Type. Type può essere graph o edges.
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

% Fatti
heap(heap, 10).
heap_entry(heap, 1, 1, 1).
heap_entry(heap, 2, 2, 2).
heap_entry(heap, 3, 4, 4).
heap_entry(heap, 4, 7, 7).
heap_entry(heap, 5, 3, 3).
heap_entry(heap, 6, 10, 10).
heap_entry(heap, 7, 9, 9).
heap_entry(heap, 8, 16, 16).
heap_entry(heap, 9, 8, 8).
heap_entry(heap, 10, 14, 14).

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
    K > PK,
    !.

heapify_insert(H, I) :-
    P is floor(I / 2),
    heap_entry(H, I, K, V),
    heap_entry(H, P, PK, PV),
    K < PK,
    retract(heap_entry(H, I, K, V)),
    retract(heap_entry(H, P, PK, PV)),
    assert(heap_entry(H, P, K, V)),
    assert(heap_entry(H, I, PK, PV)),
    heapify_insert(H, P).

% Il predicato è vero quando la coppia K, V con K minima, è rimossa dallo heap H.
% TODO: In certe situazioni sembra dare più di una scelta, che non dovrebbe esserci e che comunque fallisce sempre
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
    YK < XK,
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

swap(_, I, Smallest) :-
    compare(=, Smallest, I),
    !.

swap(H, I, Smallest) :-
    Smallest \= I,
    !,
    retract(heap_entry(H, Smallest, SmallestK, SmallestV)),
    retract(heap_entry(H, I, K, V)),
    assert(heap_entry(H, I, SmallestK, SmallestV)),
    assert(heap_entry(H, Smallest, K, V)),
    heapify(H, Smallest).

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

% Minimum Spanning Trees
