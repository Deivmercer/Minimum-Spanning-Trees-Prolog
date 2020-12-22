:- use_module(library(csv)).

%%% Graph
% Predicati definiti dinamicamente
:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.

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
    vertex(G, V),
    !.

new_vertex(G, V) :-
    graph(G),
    assert(vertex(G, V)),
    !.

% Questo predicato è vero quando Vs è una lista contenente tutti i vertici di G.
graph_vertices(G, Vs) :-
    graph(G),
    findall(V, vertex(G, V), Vs).

% Questo predicato stampa alla console dell’interprete Prolog una lista dei vertici del grafo G.
list_vertices(G) :-
    graph(G),
    listing(vertex(G, _)).

% Aggiunge un arco del grafo G alla base dati Prolog.
new_arc(G, U, V, Weight) :-
    arc(G, U, V, _),
    !,
    retract(arc(G, U, V, _)),
    assert(arc(G, U, V, Weight)).

new_arc(G, U, V, Weight) :-
    arc(G, V, U, _),
    !,
    retract(arc(G, V, U, _)),
    assert(arc(G, U, V, Weight)).

new_arc(G, U, V, Weight) :-
    not(arc(G, U, V, _)),
    not(arc(G, V, U, _)),
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
read_graph(G, FileName) :-
    csv_read_file(FileName, Rows, [functor(arc), arity(3), separator(0'\t)]),
    new_graph(G),
    maplist(assert_results(G), Rows).

assert_results(G, Rows) :-
    Rows =.. [P, U, V, W],
    new_vertex(G, U),
    new_vertex(G, V),
    Arc =.. [P, G, U, V, W],
    assert(Arc).

% Questo predicato è vero quando G viene scritto sul file FileName secondo il valore dell’argomento Type. Type può essere graph o edges.
write_graph(G, FileName) :-
   write_graph(G, FileName, graph).

write_graph(G, FileName, graph) :-
    atom(G),
    !,
    graph_arcs(G, Es),
    maplist(prepare_output, Es, Rows),
    csv_write_file(FileName, Rows, [separator(0'\t)]).
 
write_graph(G, FileName, edges) :-
    compound(G),
    !,
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

% Il predicato è vero quando l’elemento V è inserito nello heap H con chiave K.
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
heap_extract(H, K, V) :-
    heap_head(H, K, V),
    heap_has_size(H, S),
    compare(=, S, 1),
    !,
    retract(heap_entry(H, 1, K, V)),
    retract(heap(H, S)),
    assert(heap(H, 0)).

heap_extract(H, K, V) :-
    heap_head(H, K, V),
    heap_has_size(H, S),
    S > 1,
    !,
    retract(heap_entry(H, 1, K, V)),
    retract(heap_entry(H, S, SK, SV)),
    assert(heap_entry(H, 1, SK, SV)),
    retract(heap(H, S)),
    S1 is S - 1,
    assert(heap(H, S1)),
    heapify(H, 1).

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
modify_key(H, NewKey, OldKey, V) :-
    heap_entry(H, SV, OldKey, V),
    heap_has_size(H, S),
    S > 1,
    !,
    retract(heap_entry(H, SV, OldKey, V)),
    assert(heap_entry(H, SV, NewKey, V)),
    heapify_insert(H, SV).

% Il predicato stampa sulla console Prolog lo stato interno dello heap
list_heap(H) :-
    heap(H, _),
    listing(heap(H, _)),
    listing(heap_entry(H, _, _, _)).

%%% Minimum Spanning Trees
% Predicati definiti dinamicamente
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.

% Dopo la prova di questo predicato, la base-dati Prolog ha al suo interno i predicati vertex_key(G, V, K) per ogni V appartenente a G; 
% la base-dati Prolog contiene anche i predicati vertex_previous(G, V, U) per ogni V, ottenuti durante le iterazioni dell’algoritmo di Prim.
mst_prim(G, Source) :-
    vertex(G, Source),
    graph_vertices(G, Vs),
    new_heap(G),
    mst_prepare_queue(G, Vs),
    modify_key(G, 0, inf, Source),
    mst_build_tree(G).

mst_prepare_queue(_, []) :-
    !.

mst_prepare_queue(G, [V | Vs]) :-
    heap_insert(G, inf, V),
    assert(vertex_key(G, V, inf)),
    assert(vertex_previous(G, V, nil)),
    mst_prepare_queue(G, Vs).

mst_build_tree(G) :-
    heap_empty(G),
    !.

mst_build_tree(G) :-
    heap_not_empty(G),
    !,
    heap_extract(G, _, V),
    adjs(G, V, As),
    mst_adjs(G, V, As),
    mst_build_tree(G).

mst_adjs(_, _, []) :-
    !.

mst_adjs(G, V, [A | As]) :-
    A =.. [vertex, G, AV],
    not(heap_entry(G, _, _, AV)),
    !,
    mst_adjs(G, V, As).

mst_adjs(G, V, [A | As]) :-
    A =.. [vertex, G, AV],
    heap_entry(G, _, _, AV),
    arc(G, V, AV, WNew),
    vertex_key(G, AV, WOld),
    WNew >= WOld,
    !,
    mst_adjs(G, V, As).

mst_adjs(G, V, [A | As]) :-
    A =.. [vertex, G, AV],
    heap_entry(G, _, _, AV),
    arc(G, AV, V, WNew),
    vertex_key(G, AV, WOld),
    WNew >= WOld,
    !,
    mst_adjs(G, V, As).

mst_adjs(G, V, [A | As]) :-
    A =.. [vertex, G, AV],
    heap_entry(G, _, AK, AV),
    arc(G, V, AV, WNew),
    vertex_key(G, AV, WOld),
    WNew < WOld,
    !,
    retract(vertex_key(G, AV, WOld)),
    retract(vertex_previous(G, AV, _)),
    assert(vertex_key(G, AV, WNew)),
    assert(vertex_previous(G, AV, V)),
    modify_key(G, WNew, AK, AV),
    mst_adjs(G, V, As).

mst_adjs(G, V, [A | As]) :-
    A =.. [vertex, G, AV],
    heap_entry(G, _, AK, AV),
    arc(G, AV, V, WNew),
    vertex_key(G, AV, WOld),
    WNew < WOld,
    !,
    retract(vertex_key(G, AV, WOld)),
    retract(vertex_previous(G, AV, _)),
    assert(vertex_key(G, AV, WNew)),
    assert(vertex_previous(G, AV, V)),
    modify_key(G, WNew, AK, AV),
    mst_adjs(G, V, As).

% Questo predicato è vero quando PreorderTree è una lista degli archi del MST ordinata secondo un attraversamento preorderdello stesso, 
% fatta rispetto al peso dell’arco.
% mst_get(G, Source, PreorderTree)