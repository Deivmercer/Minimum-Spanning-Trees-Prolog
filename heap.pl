:- module(heap,
        [ heap/2,
          heap_entry/4,
          new_heap/1,
          delete_heap/1,
          heap_has_size/2,
          heap_empty/1,
          heap_not_empty/1,
          heap_head/3,
          heap_insert/3,
          heapify_insert/2,
          heap_extract/3,
          heapify/2,
          smallest/4,
          swap/3,
          modify_key/4,
          list_heap/1 ]).

/** <module> Heap

Libreria per la gestione di MinHeap.

@author Davide Costantini
@version 1.0
*/

%!  heap(+H:string, +S:int) is det.
%
%   @arg H  Nome dell'heap
%   @arg S  Dimensione attuale dell'heap. 
%
%   Rappresentazione di uno heap.

:- dynamic heap/2.

%!  heap_entry(+H:string, +P:int, +K:int, +V:string) is det.
%!  heap_entry(+H:string, +P:int, +K:int, +V:int) is det.
%
%   @arg H  Nome dell'heap
%   @arg P  Posizione dell'elemento all'interno dell'heap
%   @arg K  Chiave dell'elemento
%   @arg V  Valore dell'elemento 
%
%   Rappresentazione di un elemento all'interno dell'heap.
%   Il manentimento della proprieta' di heap viene effettuato in base alla 
%   chiave.

:- dynamic heap_entry/4.

%!  new_heap(+H:string) is det.
%
%   @arg H  Nome dell'heap
%
%   True se H rappresenta uno heap. Se al momento della chiamata H non 
%   rappresenta uno heap allora viene asserito un nuovo heap.

new_heap(H) :-
    heap(H, _),
    !.

new_heap(H) :-
    assert(heap(H, 0)),
    !.

%!  delete_heap(+H:string) is det.
%
%   @arg H  Nome dell'heap
%
%   True se e' stato possibile ritrattare tutti i fatti relativi all'heap H
%   (compresi gli heap_entry).

delete_heap(H) :-
    retract(heap(H, _)),
    retractall(heap_entry(H, _, _, _)).

%!  heap_has_size(+H:string, -S:int) is det.
%
%   @arg H  Nome dell'heap
%   @arg S  Dimensione dell'heap
%
%   True se S e' la dimensione dell'heap H.

heap_has_size(H, S) :-
    heap(H, S).

%!  heap_empty(+H:string) is det.
%
%   @arg H  Nome dell'heap
%
%   True se l'heap H e' vuoto.

heap_empty(H) :-
    heap(H, 0).

%!  heap_not_empty(+H:string) is det.
%
%   @arg H  Nome dell'heap
%
%   True se l'heap H non e' vuoto.

heap_not_empty(H) :-
    heap(H, S),
    S > 0.

%!  heap_head(+H:string, -K:int, -V:string) is det.
%!  heap_head(+H:string, -K:int, -V:int) is det.
%
%   @arg H  Nome dell'heap
%   @arg K  Chiave dell'elemento in testa all'heap
%   @arg V  Valore dell'elemento in testa all'heap
%
%   True se K e V sono rispettivamente la chiave ed il valore dell'elemento in
%   testa all'heap.

heap_head(H, K, V) :-
    heap_not_empty(H),
    heap_entry(H, 1, K, V).

%!  heap_insert(+H:string, +K:int, +V:string) is det.
%!  heap_insert(+H:string, +K:int, +V:int) is det.
%
%   @arg H  Nome dell'heap
%   @arg K  Chiave dell'elemento da inserire nell'heap
%   @arg V  Valore dell'elemento da inserire nell'heap
%
%   True se e' stato possibile asserire un nuovo heap_entry con chiave K e
%   valore V e l'heap soddisfa ancora la proprieta' di heap. Se H non
%   rappresenta un heap, allora verrà creato un nuovo heap.

heap_insert(H, K, V) :-
    new_heap(H),
    retract(heap(H, S)),
    S1 is S + 1,
    assert(heap(H, S1)),
    assert(heap_entry(H, S1, K, V)),
    heapify_insert(H, S1).

%!  heapify_insert(+H:string, +I:int) is det.
%
%   @arg H  Nome dell'heap
%   @arg I  Indice della testa del sottoalbero
%
%   True se l'heap H soddisfa la proprieta' di heap.

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

%!  heap_extract(+H:string, -K:int, -V:string) is det.
%!  heap_extract(+H:string, -K:int, -V:int) is det.
%
%   @arg H  Nome dell'heap
%   @arg K  Chiave dell'elemento in testa all'heap
%   @arg V  Valore dell'elemento in testa all'heap
%
%   True se e' stato possibile estrarre l'elemento in testa all'heap, K e V sono
%   rispettivamente la sua chiave ed il suo valore e l'heap H soddisfa ancora la
%   proprieta' di heap.

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

%!  heapify(+H:string, +I:int) is det.
%
%   @arg H  Nome dell'heap
%   @arg I  Indice della testa del sottoalbero
%
%   True se il sottoalbero dell'heap H con radice nell'elemento I soddisfa la 
%   proprieta' di heap.

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

%!  smallest(+H:string, +X:int, +Y:int, -Smallest:int) is det.
%
%   @arg H          Nome dell'heap
%   @arg X          Posizione del primo elemento
%   @arg Y          Posizione del secondo elemento
%   @arg Smallest   Posizione dell'elemento piu' piccolo tra i due
%
%   True se Smallest e' l'elemento con chiave piu' piccola tra quello in 
%   posizione X e quello in posizione Y all'interno dell'heap H.

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

%!  swap(+H:string, -X:int, -Y:int) is det.
%
%   @arg H  Nome dell'heap
%   @arg X  Posizione del primo elemento
%   @arg Y  Posizione del secondo elemento
%
%   True se e' stato possibile scambiare gli elementi in posizione X ed Y
%   all'interno dell'heap H e se l'heap soddisfa ancora la proprieta' di heap.

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

%!  modify_key(+H:string, +NewKey:int, +OldKey:int, +V:string) is det.
%!  modify_key(+H:string, +NewKey:int, +OldKey:int, +V:int) is det.
%
%   @arg H      Nome dell'heap
%   @arg NewKey Nuova chiave da assegnare all'elemento con chiave OldKey e 
%               valore V
%   @arg OldKey Chiave dell'elemento da modificare
%   @arg V      Valore dell'elemento da modificare
%
%   True se e' stato possibile modificare l'elemento dell'heap H con chiave 
%   OldKey e valore V impostando NewKey come nuova chiave e l'heap soddisfa 
%   ancora la proprieta' di heap.

modify_key(H, NewKey, OldKey, V) :-
    heap_entry(H, _, OldKey, V),
    compare(=, NewKey, OldKey),
    !.

modify_key(H, NewKey, OldKey, V) :-
    heap_entry(H, SV, OldKey, V),
    NewKey \= OldKey,
    heap_has_size(H, S),
    compare(=, S, 1),
    !,
    retract(heap_entry(H, SV, OldKey, V)),
    assert(heap_entry(H, SV, NewKey, V)).

modify_key(H, NewKey, OldKey, V) :-
    heap_entry(H, SV, OldKey, V),
    NewKey < OldKey,
    heap_has_size(H, S),
    S > 1,
    !,
    retract(heap_entry(H, SV, OldKey, V)),
    assert(heap_entry(H, SV, NewKey, V)),
    heapify_insert(H, SV).

modify_key(H, NewKey, OldKey, V) :-
    heap_entry(H, SV, OldKey, V),
    NewKey > OldKey,
    heap_has_size(H, S),
    S > 1,
    !,
    retract(heap_entry(H, SV, OldKey, V)),
    assert(heap_entry(H, SV, NewKey, V)),
    heapify(H, SV).

%!  list_heap(+H:string) is det.
%
%   @arg H  Nome dell'heap
%
%   Stampa tutte le heap_entry relative all'heap H.

list_heap(H) :-
    heap(H, _),
    listing(heap(H, _)),
    listing(heap_entry(H, _, _, _)).
