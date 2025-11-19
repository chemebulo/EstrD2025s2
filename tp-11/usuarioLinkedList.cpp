#include <iostream>
#include "LinkedList.h"
using namespace std;

int sumatoria(LinkedList xs){
// PROPÓSITO: Devuelve la suma de to dos los elementos.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    ListIterator ixs = getIterator(xs);
    int resultado = 0;

    while(!atEnd(ixs)){
        resultado += current(ixs);
        Next(ixs);
    }

    DisposeIterator(ixs);

    return resultado;
}

void Sucesores(LinkedList xs){
// PROPÓSITO: Incrementa en uno todos los elementos.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    ListIterator ixs = getIterator(xs);

    while(!atEnd(ixs)){
        SetCurrent(current(ixs) + 1, ixs);
        Next(ixs);
    }

    DisposeIterator(ixs);
}

bool pertenece(int x, LinkedList xs){
// PROPÓSITO: Indica si el elemento pertenece a la lista.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    ListIterator ixs = getIterator(xs);

    while(!atEnd(ixs) && x != current(ixs)){
        Next(ixs);
    }

    DisposeIterator(ixs);

    return !atEnd(ixs);
}

int apariciones(int x, LinkedList xs){
// PROPÓSITO: Indica la cantidad de elementos iguales a x.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    ListIterator ixs = getIterator(xs);
    int apariciones = 0;

    while(!atEnd(ixs)){
        if(current(ixs) == x){
            apariciones++;
        }
        Next(ixs);
    }

    DisposeIterator(ixs);

    return apariciones;
}

int minimo(LinkedList xs){
// PROPÓSITO: Devuelve el elemento más chico de la lista.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    ListIterator ixs = getIterator(xs);
    
    if(atEnd(ixs)){
        throw new runtime_error("No se puede obtener el minimo de una LinkedList vacía.");
    }

    int minimo = current(ixs);
    Next(ixs);

    while(!atEnd(ixs)){
        if(current(ixs) < minimo){
            minimo = current(ixs);
        }
        Next(ixs);
    }
    
    DisposeIterator(ixs);

    return minimo;
}

LinkedList copy(LinkedList xs){
// PROPÓSITO: Dada una lista genera otra con los mismos elementos, en el mismo orden.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    ListIterator ixs = getIterator(xs);
    LinkedList ys = nil();

    while(!atEnd(ixs)){
        Snoc(current(ixs), ys);
        Next(ixs);
    }

    DisposeIterator(ixs);

    return ys;
}

/* INTERFAZ DE LINKEDLIST:
    * LinkedList nil()
    * bool isEmpty(LinkedList xs)
    * int head(LinkedList xs)
    * void Cons(int x, LinkedList xs)
    * void Tail(LinkedList xs)
    * int length(LinkedList xs)
    * void Snoc(int x, LinkedList xs)
    * void DestroyL(LinkedList xs)
    * ListIterator getIterator(LinkedList xs)
    * int current(ListIterator ixs)
    * void SetCurrent(int x, ListIterator ixs)
    * void Next(ListIterator ixs)
    * bool atEnd(ListIterator ixs)
    * void DisposeIterator(ListIterator ixs)
*/

int main(){
    return 0;
}