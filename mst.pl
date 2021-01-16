:- module(mst,
        [ vertex_key/3,
          vertex_previous/3,
          delete_mst/1,
          mst_prim/2,
          mst_prepare_queue/2,
          mst_build_tree/1,
          mst_adjs/3,
          mst_get/3,
          mst_childs_weight/3,
          mst_preorder_tree/4 ]).

:- use_module(graph).
:- use_module(heap).

/** <module> Minimum Spanning Tree

Libreria per il calcolo del Minimum Spanning Tree di un grafo.

@author Davide Costantini
@version 1.0
*/

%!  vertex_key(+G:string, +V:string, +K:int) is det. 
%
%   @arg G  Nome del grafo di cui si vuole calcolare l'albero
%   @arg V  Nome del vertice
%   @arg K  Distanza dal vertice che lo precede nell'MST 
%
%   Rappresentazione di un nodo dell'MST.

:- dynamic vertex_key/3.

%!  vertex_previous(+G:string, +V:string, +U:string) is det.
%
%   @arg G  Nome del grafo di si vuole calcolare l'albero
%   @arg V  Nome del vertice
%   @arg U  Nome del vertice che lo precede nell'MST 
%
%   Rappresentazione delle precedenze all'interno dell'MST.

:- dynamic vertex_previous/3.

%!  delete_mst(+G:string) is det.
%
%   @arg G  Nome del grafo
%
%   True se e' stato possibile ritrattare tutti i fatti relativi all'MST.

delete_mst(G) :-
    retractall(vertex_key(G, _, _)),
    retractall(vertex_previous(G, _, _)).

%!  mst_prim(+G:string, +Source:string) is det.
%
%   @arg G      Nome del grafo di si vuole calcolare l'albero
%   @arg Source Nome del nodo di partenza dell'albero 
%
%   True se e' stato possibile calcolare l'MST del grafo specificato, che verra'
%   memorizzato nella base di conoscenza di Prolog all'interno dei fatti
%   vertex_key e vertex_previous. Se al momento della chiamata esiste gia' un
%   MST per il grafo G allora verrà sovrascritto.

mst_prim(G, Source) :-
    vertex(G, Source),
    delete_mst(G),
    graph_vertices(G, Vs),
    new_heap(G),
    mst_prepare_queue(G, Vs),
    modify_key(G, 0, inf, Source),
    mst_build_tree(G).

%!  mst_prepare_queue(+G:string, +Vs:list) is det.
%
%   @arg G  Nome del grafo di si vuole calcolare l'albero
%   @arg Vs La lista dei vertici del grafo.
%
%   True se e' stato possibile asserire i fatti vertex_key e vertex_previous
%   iniziali per ogni vertice del grafo.

mst_prepare_queue(_, []) :-
    !.

mst_prepare_queue(G, [V | Vs]) :-
    V =.. [_, G, N],
    heap_insert(G, inf, N),
    assert(vertex_key(G, N, inf)),
    assert(vertex_previous(G, N, nil)),
    mst_prepare_queue(G, Vs).

%!  mst_build_tree(+G:string) is det.
%
%   @arg G  Nome del grafo di si vuole calcolare l'albero
%
%   True se e' stato possibile calcolare l'MST del grafo specificato, che verra'
%   memorizzato nella base di conoscenza di Prolog.

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

%!  mst_adjs(+G:string, +V:string, +As:list) is det.
%
%   @arg G  Nome del grafo di si vuole calcolare l'albero
%   @arg V  Nome del vertice che si sta considerando
%   @arg As Lista dei vertici adiacenti a V all'interno del grafo G
%
%   True se e' stato possibile calcolare le posizioni all'interno dell'MST dei
%   vertici adiacenti a V.

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

%!  mst_get(+G:string, +Source:string, -PreorderTree:list) is det.
%
%   @arg G              Nome del grafo di si vuole calcolare l'albero
%   @arg Source         Nome del nodo di partenza dell'albero (o sottoalbero)
%   @arg PreorderTree   Lista degli archi ottenuti leggendo l'MST in preordine
%
%   True se e' stato possibile leggere l'MST in preordine rispetto al peso degli 
%   archi (se due archi hanno pari peso vengono ordinati lessicograficamente) e
%   PreorderTree contiene la lista degli archi.

mst_get(G, Source, _) :-
    findall(C, vertex_previous(G, C, Source), []),
    !.

mst_get(G, Source, PreorderTree) :-
    findall(C, vertex_previous(G, C, Source), Cs),
    mst_childs_weight(G, Cs, Us),
    sort(1, @=<, Us, STemp),
    sort(2, =<, STemp, Sorted),
    mst_preorder_tree(G, Source, Sorted, Tree),
    flatten(Tree, PreorderTree).

%!  mst_childs_weight(+G:string, +Cs:list, -Us:list) is det.
%
%   @arg G  Nome del grafo di si vuole calcolare l'albero
%   @arg Cs Lista dei figli del nodo corrente
%   @arg Us Lista dei figli associati al loro peso
%
%   True se Us contiene la lista dei figli del nodo corrente associato al loro
%   peso all'interno dell'MST. L'associazione e' rappresentata come child(C, W),
%   dove C e' il nome del nodo figlio e W e' il peso.

mst_childs_weight(_, [], []) :- 
    !.

mst_childs_weight(G, [C | Cs], [U | Us]) :- 
    !,
    vertex_key(G, C, W),
    U =.. [child, C, W],
    mst_childs_weight(G, Cs, Us).

%!  mst_preorder_tree(+G:string, +P:string, +Us:list, -Ts:list) is det.
%
%   @arg G  Nome del grafo di si vuole calcolare l'albero
%   @arg P  Nome del nodo di partenza dell'albero (o sottoalbero)
%   @arg Us Lista dei nodi figli di P
%   @arg Ts Lista degli archi ottenuti leggendo l'MST in preordine
%
%   True se Ts contiene la lista degli archi ottenuti leggendo l'MST.

mst_preorder_tree(_, _, [], []) :- 
    !.

mst_preorder_tree(G, P, [U | Us], [A | Ts]) :-
    U =.. [_, C, W],
    vertex_previous(G, _, C),
    arc(G, P, C, W),
    !,
    T =.. [arc, G, P, C, W],
    mst_get(G, C, Bs),
    append([T], Bs, A),
    mst_preorder_tree(G, P, Us, Ts).

mst_preorder_tree(G, P, [U | Us], [T | Ts]) :-
    U =.. [_, C, W],
    not(vertex_previous(G, _, C)),
    arc(G, P, C, W),
    !,
    T =.. [arc, G, P, C, W],
    mst_preorder_tree(G, P, Us, Ts).
