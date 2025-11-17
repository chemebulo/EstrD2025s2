#include <iostream>
#include "ArrayList.h"
using namespace std;

ArrayList newArrayList(){
    // PROPÓSITO: Crea una lista con 0 elementos.
    // NOTA: Empezar el ArrayList con capacidad 16.
    ArrayList al = new ArrayListSt();
    al->cantidad = 0;
    al->capacidad = 16;
    al->elementos = new int[al->capacidad];
    return al;
}

void validarCapacidad(int n){
    if(n <= 0){
        throw new runtime_error("No es posible crear un array con dicha capacidad.");
    }
}

ArrayList newArrayListWith(int capacidad){
// PROPÓSITO: Crea una lista con 0 elementos y una capacidad dada por parámetro.
    validarCapacidad(capacidad);
    ArrayList al = new ArrayListSt();
    al->cantidad = 0;
    al->capacidad = capacidad;
    al->elementos = new int[al->capacidad];
    return al;
}

int lengthAL(ArrayList xs){
// PROPÓSITO: Devuelve la cantidad de elementos existentes.
    return xs->cantidad;
}

void validarAccesoAIndice(int i, ArrayList xs){
    if(i < 0 || i >= xs->cantidad){
        throw new runtime_error("No es posible acceder al indice dado.");
    }
}

int get(int i, ArrayList xs){
// PROPÓSITO: Devuelve el iésimo elemento de la lista.
// PRECONDICIÓN: El ArrayList dado debe tener al menos i elementos.
    validarAccesoAIndice(i, xs);
    return xs->elementos[i];
}

void set(int i, int x, ArrayList xs){
// PROPÓSITO: Reemplaza el iésimo elemento por otro dado.
// PRECONDICIÓN: El ArrayList dado debe tener al menos i elementos.
    validarAccesoAIndice(i, xs);
    xs->elementos[i] = x;
}

void resize(int capacidad, ArrayList xs){
// PROPÓSITO: Decrementa o aumenta la capacidad del array.
// NOTA: En caso de decrementarla, se pierden los elementos del final de la lista.
    int* elems = new int[capacidad];

    int cantidad = (capacidad < xs->cantidad) ? capacidad : xs->cantidad;

    for(int i = 0; i < cantidad; i++){
        elems[i] = xs->elementos[i];
    }
    
    xs->cantidad = cantidad;
    delete[] xs->elementos;
    xs->elementos = elems;
    xs->capacidad = capacidad;
}

void add(int x, ArrayList xs){
// PROPÓSITO: Agrega un elemento al final de la lista.

}

void remove(ArrayList xs){
// PROPÓSITO: Borra el último elemento de la lista.

}