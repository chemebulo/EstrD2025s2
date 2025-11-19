#include <iostream>
#include "LinkedList.h"
using namespace std;

struct NodoS {
    /* INVARIANTES DE REPRESENTACIÓN:
        * Ninguna.
    */
    int value;   // Valor del nodo.
    NodoS* next; // Puntero al siguiente nodo.
};

typedef NodoS* NodoSS; // INV. REP.: El puntero no es NULL.

struct SetSt {
    /* INVARIANTES DE REPRESENTACIÓN:
        * Size debe representar la cantidad de nodos a recorrer hasta alcanzar NULL desde first.
        * Recorriendo desde el nodo first hasta NULL, no puede haber dos nodos con el mismo value.
    */
    int size;     // Cantidad de elementos diferentes.
    NodoS* first; // Puntero al primer nodo.
};

typedef SetSt* Set; // INV. REP.: El puntero no es NULL.

// ########################################################################################################################## //

Set emptyS();
// PROPÓSITO: Crea un conjunto vacío.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

bool isEmptyS(Set s);
// PROPÓSITO: Indica si el conjunto está vacío.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

bool belongsS(int x, Set s);
// PROPÓSITO: Indica si el elemento pertenece al conjunto.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).

void AddS(int x, Set s);
// PROPÓSITO: Agrega un elemento al conjunto.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).

void RemoveS(int x, Set s);
// PROPÓSITO: Quita un elemento dado.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).

int sizeS(Set s);
// PROPÓSITO: Devuelve la cantidad de elementos.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

LinkedList setToList(Set s);
// PROPÓSITO: Devuelve una lista con los elementos del conjunto.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).

void DestroyS(Set s);
// PROPÓSITO: Libera la memoria o cupada por el conjunto.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).

// ########################################################################################################################## //