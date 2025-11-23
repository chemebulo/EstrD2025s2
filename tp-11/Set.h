#include <iostream>
#include "LinkedList.h"
using namespace std;

struct NodoS;

struct SetSt;
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
// PROPÓSITO: Libera la memoria ocupada por el conjunto.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).

// ########################################################################################################################## //