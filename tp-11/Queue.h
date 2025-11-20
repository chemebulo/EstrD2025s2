#include <iostream>
using namespace std;

struct NodoQ {
    /* INVARIANTES DE REPRESENTACIÓN:
        * Ninguno.
    */
    int value;   // Valor del nodo.
    NodoQ* next; // Puntero al siguiente nodo.
};

typedef NodoQ* NodoQQ; // INV. REP.: El puntero no es NULL.

struct QueueSt {
    /* INVARIANTES DE REPRESENTACIÓN:
        * Size es la cantidad de nodos a recorrer desde first hasta llegar a NULL.
        * Si last != NULL, entonces last->next es NULL, y además first != NULL.
    */
    int size;     // Cantidad de elementos.
    NodoQ* first; // Puntero al primer nodo.
    NodoQ* last;  // Puntero al ultimo nodo.
};

typedef QueueSt* Queue; // INV. REP.: El puntero no es NULL.

// ########################################################################################################################## //

Queue emptyQ();
// PROPÓSITO: Crea una cola vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

bool isEmptyQ(Queue q);
// PROPÓSITO: Indica si la cola está vacía.
// PRECONDICIÓN: La Queue no está vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

int firstQ(Queue q);
// PROPÓSITO: Devuelve el primer elemento.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

void Enqueue(int x, Queue q);
// PROPÓSITO: Agrega un elemento al final de la cola.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

void Dequeue(Queue q);
// PROPÓSITO: Quita el primer elemento de la cola.
// PRECONDICIÓN: La Queue no está vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

int lengthQ(Queue q);
// PROPÓSITO: Devuelve la cantidad de elementos de la cola.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

void MergeQ(Queue q1, Queue q2);
// PROPÓSITO: Anexa Q2 al final de Q1, liberando la memoria inservible de Q2 en el proceso.
// NOTA: Si bien se libera memoria de Q2, no necesariamente la de sus nodos.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

void DestroyQ(Queue q);
// PROPÓSITO: Libera la memoria ocupada por la cola.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).

// ########################################################################################################################## //