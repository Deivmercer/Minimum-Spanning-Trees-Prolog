:- module(graph,
        [ graph/1,
          vertex/2,
          arc/4,
          new_graph/1,
          delete_graph/1,
          new_vertex/2,
          graph_vertices/2,
          list_vertices/1,
          new_arc/3,
          new_arc/4,
          graph_arcs/2,
          vertex_neighbors/3,
          adjs/3,
          list_arcs/1,
          list_graph/1,
          read_graph/2,
          assert_results/2,
          write_graph/2,
          write_graph/3,
          prepare_output/2 ]).

:- use_module(library(csv)).
:- use_module(mst).

/** <module> Graph

Libreria per la gestione di grafi.

@author Davide Costantini
@version 1.0
*/

%!  graph(+G:string) is det.
%
%   @arg G  Nome del grafo
%
%   Rappresentazione di un grafo.

:- dynamic graph/1.

%!  vertex(+G:string, +V:string) is det.
%
%   @arg G  Nome del grafo
%   @arg V  Nome del vertice
%
%   Rappresentazione di un vertice.

:- dynamic vertex/2.

%!  arc(+G:string, +U:string, +V:string, +W:int) is det.
%
%   @arg G  Nome del grafo
%   @arg V  Nome del vertice sorgente
%   @arg V  Nome del vertice destinazione
%   @arg K  Peso dell'arco
%
%   Rappresentazione di un nodo dell'MST.

:- dynamic arc/4.

%!  new_graph(+G:string) is det.
%
%   @arg G  Nome del grafo
%
%   True se G rappresenta un grafo. Se al momento della chiamata G non
%   rappresenta un grafo allora verra' asserito un nuovo grafo.

new_graph(G) :- 
    graph(G), 
    !.

new_graph(G) :- 
    assert(graph(G)), 
    !.

%!  delete_graph(+G:string) is det.
%
%   @arg G  Nome del grafo
%
%   True se e' stato possibile ritrattare tutti i fatti relativi al grafo
%   (compresi vertici, archi e fatti relativi al suo MST).

delete_graph(G) :-
    retract(graph(G)),
    retractall(vertex(G, _)),
    retractall(arc(G, _, _, _)),
    delete_mst(G).

%!  new_vertex(+G:string, +V:string) is det.
%
%   @arg G  Nome del grafo
%   @arg V  Nome del vertice
%
%   True se V rappresenta un vertice del grafo V. Se V non rappresenta un 
%   vertice del grafo V, allora verra' asserito un nuovo vertice. Se G non
%   rappresenta un grafo, allora verrà creato un nuovo grafo.

new_vertex(G, V) :-
    vertex(G, V),
    !.

new_vertex(G, V) :-
    new_graph(G),
    assert(vertex(G, V)),
    !.

%!  graph_vertices(+G:string, -Vs:list) is det.
%
%   @arg G  Nome del grafo
%   @arg Vs Lista dei vertici del grafo
%
%   True se Vs contiene tutti i vertici relativi al grafo G.

graph_vertices(G, Vs) :-
    graph(G),
    findall(vertex(G, V), vertex(G, V), Vs).

%!  list_vertices(+G:string) is det.
%
%   @arg G  Nome del grafo
%
%   Stampa tutti i vertici relativi al grafo G.

list_vertices(G) :-
    graph(G),
    listing(vertex(G, _)).

%!  new_arc(+G:string, +U:string, +V:string, +W:int) is det.
%
%   @arg G  Nome del grafo
%   @arg U  Nome del vertice sorgente
%   @arg V  Nome del vertice destinazione
%   @arg W  Peso dell'arco
%
%   True se all'interno del grafo e' presente un grafo da U a V con peso W. Se
%   al momento della chiamata questo arco non esiste, allora vengono asseriti
%   gli archi da U a V e da V ad U. Se e' gia' presente un arco da U a V con
%   peso diverso da W allora l'arco viene modificato in modo che il peso sia W.
%   Se G non rappresenta un grafo, allora verrà creato un nuovo grafo.

new_arc(G, U, V, Weight) :-
    arc(G, U, V, Weight),
    !.

new_arc(G, U, V, Weight) :-
    arc(G, U, V, _),
    !,
    retract(arc(G, U, V, _)),
    retract(arc(G, V, U, _)),
    assert(arc(G, U, V, Weight)),
    assert(arc(G, V, U, Weight)).

new_arc(G, U, V, Weight) :-
    not(arc(G, U, V, _)),
    !,
    new_vertex(G, U),
    new_vertex(G, V),
    assert(arc(G, U, V, Weight)),
    assert(arc(G, V, U, Weight)).

%!  new_arc(+G:string, +U:string, +V:string) is det.
%
%   @arg G  Nome del grafo
%   @arg U  Nome del vertice sorgente
%   @arg V  Nome del vertice destinazione
%
%   True e' true la chiamata a new_arc/4 con W = 1.

new_arc(G, U, V) :- 
    new_arc(G, U, V, 1).

%!  graph_arcs(+G:string, -Es:list) is det.
%
%   @arg G  Nome del grafo
%   @arg Es Lista degli archi del grafo
%
%   True se Es contiene tutti gli archi del grafo.

graph_arcs(G, Es) :-
    graph(G),
    findall(arc(G, U, V, W), arc(G, U, V, W), Es).

%!  vertex_neighbors(+G:string, +V:string, -Ns:list) is det.
%
%   @arg G  Nome del grafo
%   @arg V  Nome del vertice destinazione
%   @arg Ns Lista degli archi che congiungono V agli altri vertici
%
%   True se Ns e' la lista degli archi che portano ai vertici N immediatamente
%   raggiungibili da V nel grafo G.

vertex_neighbors(G, V, Ns) :-
    vertex(G, V),
    findall(arc(G, V, N, W), arc(G, V, N, W), Ns).

%!  adjs(+G:string, +V:string, -Vs:list) is det.
%
%   @arg G  Nome del grafo
%   @arg V  Nome del vertice destinazione
%   @arg Vs Lista degi vertici adiacenti a V
%
%   True se Vs e' la lista dei vertici adiacenti a V nel grafo G.

adjs(G, V, Vs) :- 
    vertex(G, V),
    findall(vertex(G, U), arc(G, V, U, _), Vs).

%!  list_arcs(+G:string) is det.
%
%   @arg G  Nome del grafo
%
%   Stampa tutti gli archi relativi al grafo G.

list_arcs(G) :-
    graph(G),
    listing(arc(G, _, _, _)).

%!  list_graph(+G:string) is det.
%
%   @arg G  Nome del grafo
%
%   Stampa tutti i vertici e gli archi relativi al grafo G.

list_graph(G) :-
    list_vertices(G),
    list_arcs(G).

%!  read_graph(+G:string, +FileName:string) is det.
%
%   @arg G          Nome del grafo
%   @arg FileName   Nome del file per l'input
%
%   True se e' stato possibile leggere e memorizzare nella base di conoscenza di
%   Prolog gli archi relativi al grafo G contenuti in FileName.
%   FileName deve essere un CSV che usa \t come separatore e che contiene 
%   vertice sorgente, vertice destinazione e peso dell'arco. Se al momento della
%   chiamata esiste gia' un grafo G, allora verrà sovrascritto.

read_graph(G, FileName) :-
    delete_graph(G),
    csv_read_file(FileName, Rows, [functor(arc), arity(3), separator(0'\t)]),
    new_graph(G),
    maplist(assert_results(G), Rows).

%!  assert_results(+G:string, +Rows:string) is det.
%
%   @arg G      Nome del grafo
%   @arg Rows   Riga letta dal file
%
%   Memorizza nella base di conoscenza di Prolog i vertici e gli archi letti 
%   dal file.

assert_results(G, Rows) :-
    Rows =.. [_, U, V, W],
    new_vertex(G, U),
    new_vertex(G, V),
    new_arc(G, U, V, W).

%!  write_graph(+G:string, +FileName:string) is det.
%
%   @arg G          Nome del grafo
%   @arg FileName   Nome del file per l'output
%
%   True se e' stato possibile scrivere su FileName gli archi relativi al grafo 
%   G. La scrittura viene effettuata chiamando write_graph/3 con Type = graph.

write_graph(G, FileName) :-
   write_graph(G, FileName, graph).

%!  write_graph(+G:string, +FileName:string, +Type:string) is det.
%!  write_graph(+G:list, +FileName:string, +Type:string) is det.
%
%   @arg G          Nome del grafo o lista degli archi del grafo
%   @arg FileName   Nome del file per l'output
%   @arg Type       Modalita' di scrittura del grafo
%
%   True se e' stato possibile scrivere su FileName gli archi relativi al grafo 
%   G. 
%   Se Type = graph allora G sara' una stringa che rappresenta il nome del 
%   grafo.
%   Se Type = edges allora G sara' una lista che conterra' la lista degli archi
%   di G.
%   FileName sara' un CSV che usera' \t come separatore e che conterra' vertice 
%   sorgente, vertice destinazione e peso dell'arco.

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

%!  prepare_output(+Arc:list, -Row:string) is det.
%
%   @arg Arc    Rappresentazione di un arco
%   @arg Row    Rappresentazione di un arco senza il nome del grafo
%
%   True se Row contiene la rappresentazione dell'arco senza il nome del grafo.

prepare_output(Arc, Row) :-
    Arc =.. [P, _ | Gs],
    Row =.. [P | Gs].
