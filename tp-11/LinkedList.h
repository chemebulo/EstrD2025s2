#include <iostream>
using namespace std;

struct NodoL;

struct LinkedListSt;
typedef LinkedListSt* LinkedList; // INV.REP.: El puntero NO es NULL.

struct IteratorSt;
typedef IteratorSt* ListIterator; // INV.REP.: El puntero NO es NULL.

// ########################################################################################################################## //

LinkedList nil();
// PROPÓSITO: Crea una lista vacía
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

bool isEmpty(LinkedList xs);
// PROPÓSITO: Indica si la lista está vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

int head(LinkedList xs);
// PROPÓSITO: Devuelve el primer elemento.
// PRECONDICIÓN: La LinkedList no está vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

void Cons(int x, LinkedList xs);
// PROPÓSITO: Agrega un elemento al principio de la lista.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

void Tail(LinkedList xs);
// PROPÓSITO: Quita el primer elemento.
// PRECONDICIÓN: La LinkedList no está vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

int length(LinkedList xs);
// PROPÓSITO: Devuelve la cantidad de elementos.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

void Snoc(int x, LinkedList xs);
// PROPÓSITO: Agrega un elemento al final de la lista.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

void DestroyL(LinkedList xs);
// PROPÓSITO: Libera la memoria ocupada por la lista.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).

ListIterator getIterator(LinkedList xs);
// PROPÓSITO: Apunta el recorrido al primer elemento.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

int current(ListIterator ixs);
// PROPÓSITO: Devuelve el elemento actual en el recorrido.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

void SetCurrent(int x, ListIterator ixs);
// PROPÓSITO: Reemplaza el elemento actual por otro elemento.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

void Next(ListIterator ixs);
// PROPÓSITO: Pasa al siguiente elemento.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

bool atEnd(ListIterator ixs);
// PROPÓSITO: Indica si el recorrido ha terminado.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

void DisposeIterator(ListIterator ixs);
// PROPÓSITO: Libera la memoria ocupada por el iterador.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

// ########################################################################################################################## //