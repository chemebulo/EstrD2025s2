#include <iostream>
#include "Set.h"
using namespace std;

Set emptyS(){
// PROPÓSITO: Crea un conjunto vacío.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    Set s = new SetSt();
    s->first = NULL;
    s->size = 0;
    return s;
}

bool isEmptyS(Set s){
// PROPÓSITO: Indica si el conjunto está vacío.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    return s->size == 0;
}

bool belongsS(int x, Set s){
// PROPÓSITO: Indica si el elemento pertenece al conjunto.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    NodoSS actual = s->first;    

    while(actual != NULL && actual->value != x){
        actual = actual->next;
    }

    return actual != NULL;
}

void AddS(int x, Set s){
// PROPÓSITO: Agrega un elemento al conjunto.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    NodoSS actual = s->first;
    NodoSS nuevo = new NodoS();
    nuevo->value = x;

    while(actual != NULL && actual->value != x){
        actual = actual->next;
    }

    if(actual == NULL){
        nuevo->next = s->first;
        s->first = nuevo;
        s->size++;
    }
}

void RemoveS(int x, Set s){
// PROPÓSITO: Quita un elemento dado.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    
}

int sizeS(Set s){
// PROPÓSITO: Devuelve la cantidad de elementos.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    return s->size;
}

LinkedList setToList(Set s){
// PROPÓSITO: Devuelve una lista con los elementos del conjunto.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    
}

void DestroyS(Set s){
// PROPÓSITO: Libera la memoria o cupada por el conjunto.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    
}