[grafo]: time_grafo.png  
[primkiller]: time_primkiller.png

# Minimum Spanning Trees Prolog

Il grafo contenuto in grafo.csv è stato preso da [CLR+09] capitolo capitolo 23. Si tratta di un semplice grafo su cui testare.  
Il grafo contenuto in primkiller.csv è stato generato da un compagno di corso. È un grafo da 500.000 archi su cui si possono fare effettivi test sulla performance dell'algoritmo.  

## Benchmarks

N.B.: I tempi di esecuzione possono variare a seconda del sistema in cui il programma viene eseguito.

### Grafo

mst_prim:   0.0 secondi  
mst_get:    0.0 secondi  

![grafo]

### Primkiller

mst_prim:   3.9 secondi  
mst_get:    0.2 secondi  
![primkiller]
