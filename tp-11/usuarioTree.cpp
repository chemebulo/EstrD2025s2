#include <iostream>
#include "Tree.h"
#include "../tp-10/ArrayList.h"
using namespace std;

int sumarT(Tree t){
// PROPÓSITO: Dado un árbol binario de enteros devuelve la suma entre sus elementos.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(N).
    int sumatoria = 0;

    if(!isEmptyT(t)){
        sumatoria = sumarT(left(t)) + rootT(t) + sumarT(right(t));
    }

    return sumatoria;
}

int sizeT(Tree t){
// PROPÓSITO: Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(N).
    int tamaño = 0;

    if(!isEmptyT(t)){
        tamaño = sizeT(left(t)) + 1 + sizeT(right(t));
    }

    return tamaño;
}

bool perteneceT(int e, Tree t){
// PROPÓSITO: Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(N).
    bool pertenece = false;

    if(!isEmptyT(t)){
        pertenece = perteneceT(e, left(t)) || rootT(t) == e || perteneceT(e, right(t));
    }

    return pertenece;
}

int aparicionesT(int e, Tree t){
// PROPÓSITO: Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(N).
    int apariciones = 0;

    if(!isEmptyT(t)){
        apariciones = aparicionesT(e, left(t)) + (rootT(t) == e ? 1 : 0) + aparicionesT(e, right(t)); 
    }

    return apariciones;
}

int max(int x, int y){
// PROPÓSITO: Indica si el primer número es mayor al segundo.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    return x > y ? x : y;
}

int heightT(Tree t){
// PROPÓSITO: Dado un árbol devuelve su altura.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(N).
    int altura = 0;

    if(!isEmptyT(t)){
        altura = 1 + max(heightT(left(t)), heightT(right(t)));
    }

    return altura;
}

void addToListIn(Tree t, ArrayList xs){
// PROPÓSITO: Agrega los elementos del árbol dado a la lista dada.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(N).
    if(!isEmptyT(t)){
        addToListIn(left(t), xs);
        addToListIn(right(t), xs);
        add(rootT(t), xs);
    }
} 

ArrayList toList(Tree t){
// PROPÓSITO: Dado un árbol devuelve una lista con todos sus elementos.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(N).
    ArrayList xs = newArrayListWith(sizeT(t));
    addToListIn(t, xs);
    return xs; 
}

void addLeavesTo(Tree t, ArrayList xs){
// PROPÓSITO: Agrega todas las hojas del árbol dado a la lista dada.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(N).
    if(!isEmptyT(t)){
        if(isEmptyT(left(t)) && isEmptyT(right(t))){
            add(rootT(t), xs);
        } else {
            addLeavesTo(left(t), xs);
            addLeavesTo(right(t), xs);
        }
    }
}

ArrayList leaves(Tree t){
// PROPÓSITO: Dado un árbol devuelve los elementos que se encuentran en sus hojas.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(N).
    ArrayList xs = newArrayListWith(sizeT(t));
    addLeavesTo(t, xs);
    return xs;
}

void addLevelNTo(Tree t, int n, ArrayList xs){
// PROPÓSITO: Dados un número n, un árbol devuelve y una lista, agrega los nodos del nivel n en la lista dada.
// PRECONDICIÓN: El valor dado tiene que ser mayor igual a 0.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(N). 
    if(!isEmptyT(t)){
        if(n == 0){
            add(rootT(t), xs);
        } else {
            addLevelNTo(left(t), n-1, xs);
            addLevelNTo(right(t), n-1, xs);
        }
    }
}

ArrayList levelN(int n, Tree t){
// PROPÓSITO: Dados un número n y un árbol devuelve una lista con los nodos de nivel n.
// PRECONDICIÓN: El valor dado tiene que ser mayor igual a 0.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(N).
    if(n < 0){
        throw new runtime_error("El valor dado no es válido.");
    }

    ArrayList xs = newArrayListWith(sizeT(t));
    addLevelNTo(t, n, xs);
    return xs;
}

/* INTERFAZ DE TREE:
    * Tree emptyT()
    * Tree nodeT(int elem, Tree left, Tree right)
    * bool isEmptyT(Tree t)
    * int rootT(Tree t)
    * Tree left(Tree t)
    * Tree right(Tree t)
*/

int main(){
    return 0;
}