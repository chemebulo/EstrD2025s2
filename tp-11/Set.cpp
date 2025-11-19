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
    
    while(actual != NULL && actual->value != x){
        actual = actual->next;
    }
    
    if(actual == NULL){
        NodoSS nuevo = new NodoS();
        nuevo->value = x;
        nuevo->next = s->first;
        s->first = nuevo;
        s->size++;
    }
}

void RemoveS(int x, Set s){
// PROPÓSITO: Quita un elemento dado.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    NodoSS actual = s->first;
    NodoSS anterior = NULL;
    
    while(actual != NULL && actual->value != x){
        anterior = actual;
        actual = actual->next;
    }
    
    if(actual == NULL){
        return;
    }

    if(anterior == NULL){
        s->first = actual->next;
    } else {
        anterior->next = actual->next;
    }

    delete actual;
    s->size--;
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
    LinkedList xs = nil();
    NodoSS actual = s->first;

    while(actual != NULL){
        Snoc(actual->value, xs);
        actual = actual->next;
    }

    return xs;
}

void DestroyS(Set s){
// PROPÓSITO: Libera la memoria ocupada por el conjunto.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    NodoSS actual = s->first;
    NodoSS temp;

    while(actual != NULL){
        temp = actual;
        actual = actual->next;
        delete temp;
    }

    delete s;
}