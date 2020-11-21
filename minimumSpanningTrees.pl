:- use_module(library(csv)).

%%% Predicati definiti dinamicamente
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

%%% Graph
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

% Questo predicato è vero quanto Vs è una lista contenente tutti i vertici di G.
vertices(G, Vs) :-
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
arcs(G, Es) :-
    findall(arc(G, U, V, W), arc(G, U, V, W), Es).

% Questo predicato è vero quando V è un vertice di G e Ns è una lista contenente gli archi che portano ai vertici N immediatamente raggiungibili da V.
neighbors(G, V, Ns) :-
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
    arcs(G, Es),
    maplist(prepare_output, Es, Rows),
    csv_write_file(FileName, Rows, [separator(0'\t)]).
 
write_graph(G, FileName, edges) :-
    write(G),
    maplist(prepare_output, G, Rows),
    csv_write_file(FileName, Rows, [separator(0'\t)]).

prepare_output(Arc, Row) :-
    Arc =.. [P, _ | Gs],
    Row =.. [P | Gs].
