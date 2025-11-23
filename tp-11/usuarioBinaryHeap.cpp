#include <iostream>
#include "BinaryHeap.h"
using namespace std;

BinHeap crearHeap(int* elements, int cant){
// PROPÓSITO: Describe una heap binaria con el array de elementos dados.
// COSTO OPERACIONAL: O(N log N).
// COSTO MEMORIA: O(1).
    BinHeap h = emptyHeap();

    for(int i = 0; i < cant; i++){
        InsertH(elements[i], h);
    }

    return h;
}

/* INTERFAZ DE BINARY HEAP:
    * BinHeap emptyHeap();
    * void InsertH(int x, BinHeap h);
    * bool isEmptyHeap(BinHeap h);
    * int findMin(BinHeap h);
    * void deleteMin(BinHeap h);
*/

int main(){
    return 0;
}