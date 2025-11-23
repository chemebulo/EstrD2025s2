#include <iostream>
#include "BinaryHeap.h"
#include "limits.h"
using namespace std;

struct BinHeapHeaderSt {
    /* INVARIANTES DE REPRESENTACIÓN: 
            * curSize es menor a maxSize.
    */
    int maxSize;    // Es el tamaño máximo de la heap binaria.
    int curSize;    // Es tamaño actual de la heap binaria.
    int* elems;     // Es el array con todos los elementos de la heap binaria.
};

BinHeap emptyHeap(){
// PROPÓSITO: Describe una heap binaria vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    BinHeapHeaderSt* h = new BinHeapHeaderSt();
    h->maxSize = 16;
    h->elems = new int[h->maxSize];
    h->curSize = 0;
    h->elems[0] = INT_MIN;
    return h;
}

void AumentarEspacio(BinHeap h){
    int* elementos = new int[h->maxSize*2];

    for(int i = 0; i < h->curSize; i++){
        elementos[i] = h->elems[i];
    }

    delete[] h->elems;
    h->elems = elementos;
    h->maxSize *= 2;
}

void InsertH(int x, BinHeap h){
// PROPÓSITO: Inserta el número dado a la heap binaria dada.
// COSTO OPERACIONAL: O(log N).
// COSTO MEMORIA: O(1).
    if(h->curSize == h->maxSize - 1){
        AumentarEspacio(h);
    }

    int curNode = ++h->curSize;

    while(x < h->elems[curNode/2]){
        h->elems[curNode] = h->elems[curNode/2];
        curNode /= 2;
    }

    h->elems[curNode] = x;
}

bool isEmptyHeap(BinHeap h){
// PROPÓSITO: Indica si la heap binaria dada se encuentra vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    return h->curSize == 0;
}

int findMin(BinHeap h){
// PROPÓSITO: Describe el número mínimo de la heap binaria dada.
// PRECONDICIÓN: La heap binaria no está vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    return h->elems[1];
}

void deleteMin(BinHeap h){
// PROPÓSITO: Elimina el número mínimo de la heap binaria dada.
// PRECONDICIÓN: La heap binaria no está vacía.
// COSTO OPERACIONAL: O(log N).
// COSTO MEMORIA: O(1).
    int child;
    int curNode;
    int last = h->elems[h->curSize--];

    for(curNode = 1; curNode*2 <= h->curSize; curNode = child){
        child = curNode*2;

        if((child != h->curSize) && (h->elems[child+1] < h->elems[child])){
            child++;
        }

        if(last > h->elems[child]){
            h->elems[curNode] = h->elems[child];
        } else {
            break;
        }
    }
    h->elems[curNode] = last;    
}