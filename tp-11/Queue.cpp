#include <iostream>
#include "Queue.h"
using namespace std;

struct NodoQ {
    /* INVARIANTES DE REPRESENTACIÓN:
        * Ninguno.
    */
    int value;   // Valor del nodo.
    NodoQ* next; // Puntero al siguiente nodo.
};

struct QueueSt {
    /* INVARIANTES DE REPRESENTACIÓN:
        * Size es la cantidad de nodos a recorrer desde first hasta llegar a NULL.
        * Si last != NULL, entonces last->next es NULL, y además first != NULL.
    */
    int size;     // Cantidad de elementos.
    NodoQ* first; // Puntero al primer nodo.
    NodoQ* last;  // Puntero al ultimo nodo.
};

// ########################################################################################################################## //

Queue emptyQ(){
// PROPÓSITO: Crea una cola vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    QueueSt* q = new QueueSt();
    q->size = 0;
    q->first = NULL;
    q->last = NULL;
    return q;
}

bool isEmptyQ(Queue q){
// PROPÓSITO: Indica si la cola está vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    return q->size == 0;
}

int firstQ(Queue q){
// PROPÓSITO: Devuelve el primer elemento.
// PRECONDICIÓN: La Queue no está vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    if(q->size == 0){
        throw new runtime_error("La Queue está vacía.");
    }
    return q->first->value;
}

void Enqueue(int x, Queue q){
// PROPÓSITO: Agrega un elemento al final de la cola.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    NodoQ* nuevo = new NodoQ();
    nuevo->value = x;
    nuevo->next = NULL;

    if(q->last == NULL){
        q->first = nuevo;
    } else{ 
        q->last->next = nuevo;
    }
    
    q->last = nuevo;
    q->size++;
}

void Dequeue(Queue q){
// PROPÓSITO: Quita el primer elemento de la cola.
// PRECONDICIÓN: La Queue no está vacía.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    if(q->size == 0){
        throw new runtime_error("La Queue está vacía.");
    }

    NodoQ* temp = q->first;
    
    if(q->first->next == NULL){
        q->last = NULL;
    }
    
    q->first = temp->next;
    delete temp;
    q->size--;
}

int lengthQ(Queue q){
// PROPÓSITO: Devuelve la cantidad de elementos de la cola.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    return q->size;
}

void MergeQ(Queue q1, Queue q2){
// PROPÓSITO: Anexa Q2 al final de Q1, liberando la memoria inservible de Q2 en el proceso.
// NOTA: Si bien se libera memoria de Q2, no necesariamente la de sus nodos.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    if(q1->first == NULL){
        q1->first = q2->first;
    } else {
        q1->last->next = q2->first;
    }
    
    q1->last = q2->last;
    q1->size += q2->size;
    delete q2;
}

void DestroyQ(Queue q){
// PROPÓSITO: Libera la memoria ocupada por la cola.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    NodoQ* temp;

    while(q->first != NULL){
        temp = q->first;
        q->first = q->first->next;
        delete temp;
    }

    delete q;
}