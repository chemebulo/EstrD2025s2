#include <iostream>
#include "Tree.h"
using namespace std;

struct NodeT {
    /* INVARIANTES DE REPRESENTACIÓN:
        * Ninguno.
    */
    int elem;
    NodeT* left;
    NodeT* right;
};

// ########################################################################################################################## //

Tree emptyT(){
// PROPÓSITO: Describe un árbol vacío.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    return NULL;
}

Tree nodeT(int elem, Tree left, Tree right){
// PROPÓSITO: Describe un nodo con el elemento dado y con los subárboles dados.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    NodeT* t = new NodeT();
    t->elem = elem;
    t->left = left;
    t->right = right;
    return t;
}

bool isEmptyT(Tree t){
// PROPÓSITO: Indica si el árbol dado se encuentra vacío.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    return t == NULL;
}

int rootT(Tree t){
// PROPÓSITO: Describe el valor del árbol dado.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    return t->elem;
}

Tree left(Tree t){
// PROPÓSITO: Describe el árbol izquierdo del árbol dado.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    return t->left;
}

Tree right(Tree t){
// PROPÓSITO: Describe el árbol derecho del árbol dado.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).
    return t->right;
}