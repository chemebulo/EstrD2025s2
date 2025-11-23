#include <iostream>
using namespace std;

struct BinHeapHeaderSt;
typedef BinHeapHeaderSt* BinHeap; // INV. REP.: El puntero NO es NULL.

BinHeap emptyHeap();
// PROPÓSITO: Describe una heap binaria vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

void InsertH(int x, BinHeap h);
// PROPÓSITO: Inserta el número dado a la heap binaria dada.
// COSTO OPERACIONAL: O(log N).
// COSTO MEMORIA: O(1).

bool isEmptyHeap(BinHeap h);
// PROPÓSITO: Indica si la heap binaria dada se encuentra vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

int findMin(BinHeap h);
// PROPÓSITO: Describe el número mínimo de la heap binaria dada.
// PRECONDICIÓN: La heap binaria no está vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

void deleteMin(BinHeap h);
// PROPÓSITO: Elimina el número mínimo de la heap binaria dada.
// PRECONDICIÓN: La heap binaria no está vacía.
// COSTO OPERACIONAL: O(log N).
// COSTO MEMORIA: O(1).