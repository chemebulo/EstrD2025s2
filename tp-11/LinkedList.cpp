#include <iostream>
#include "LinkedList.h"
using namespace std;

struct NodoL {
    /* INVARIANTES DE REPRESENTACIÓN:
        * Ninguna.
    */
    int value;    // Valor del nodo.
    NodoL* next;  // Puntero al siguiente nodo.
};

struct LinkedListSt {
    /* INVARIANTES DE REPRESENTACIÓN:
        * Size es la cantidad de nodos a recorrer (nodo->next) hasta llegar a un NULL desde first.
        * First es NULL si y solo si last también lo es.
        * Si last no es NULL, entonces last->next es NULL.
    */
    int size;     // Cantidad de elementos.
    NodoL* first; // Puntero al primer nodo.
    NodoL* last;  // Puntero al último nodo.
};

struct IteratorSt {
    /* INVARIANTES DE REPRESENTACIÓN:
        * Ninguna.
    */
    NodoL* current;
};

// ########################################################################################################################## //

LinkedList nil(){
// PROPÓSITO: Crea una lista vacía
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    LinkedListSt* xs = new LinkedListSt();
    xs->first = NULL;
    xs->last = NULL;
    xs->size = 0;
    return xs;
}

bool isEmpty(LinkedList xs){
// PROPÓSITO: Indica si la lista está vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    return xs->first == NULL;
}

int head(LinkedList xs){
// PROPÓSITO: Devuelve el primer elemento.
// PRECONDICIÓN: La LinkedList no está vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    return xs->first->value;
}

void Cons(int x, LinkedList xs){
// PROPÓSITO: Agrega un elemento al principio de la lista.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    NodoL* nuevo = new NodoL();
    nuevo->value = x;
    nuevo->next = xs->first;

    if(xs->first == NULL){
        xs->last = nuevo;
    }
    
    xs->first = nuevo;
    xs->size++;
}

void Tail(LinkedList xs){
// PROPÓSITO: Quita el primer elemento.
// PRECONDICIÓN: La LinkedList no está vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    NodoL* temp = xs->first;

    if(xs->first == xs->last){
        xs->last = NULL;    
    }

    xs->first = temp->next;    
    xs->size--;
    delete temp;
}

int length(LinkedList xs){
// PROPÓSITO: Devuelve la cantidad de elementos.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    return xs->size;
}

void Snoc(int x, LinkedList xs){
// PROPÓSITO: Agrega un elemento al final de la lista.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    NodoL* nuevo = new NodoL();
    nuevo->value = x;
    nuevo->next = NULL;

    if(xs->first == NULL){
        xs->first = nuevo;
    } else {
        xs->last->next = nuevo;
    }

    xs->last = nuevo;
    xs->size++;
}

void DestroyL(LinkedList xs){
// PROPÓSITO: Libera la memoria ocupada por la lista.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    NodoL* temp;

    while(xs->first != NULL){
        temp = xs->first;
        xs->first = xs->first->next;
        delete temp;
    }

    delete xs;
}

ListIterator getIterator(LinkedList xs){
// PROPÓSITO: Apunta el recorrido al primer elemento.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    IteratorSt* ixs = new IteratorSt();
    ixs->current = xs->first;
    return ixs;
}

int current(ListIterator ixs){
// PROPÓSITO: Devuelve el elemento actual en el recorrido.
// PRECONDICIÓN: El iterador no se encuentra en el final del recorrido.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    return ixs->current->value;
}

void SetCurrent(int x, ListIterator ixs){
// PROPÓSITO: Reemplaza el elemento actual por otro elemento.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    ixs->current->value = x;
}

void Next(ListIterator ixs){
// PROPÓSITO: Pasa al siguiente elemento.
// PRECONDICIÓN: El iterador no se encuentra en el final del recorrido.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    ixs->current = ixs->current->next;
}

bool atEnd(ListIterator ixs){
// PROPÓSITO: Indica si el recorrido ha terminado.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    return ixs->current == NULL;
}

void DisposeIterator(ListIterator ixs){
// PROPÓSITO: Libera la memoria ocupada por el iterador.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    delete ixs;
}