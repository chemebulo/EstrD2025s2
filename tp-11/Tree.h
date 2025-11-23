#include <iostream>
using namespace std;

struct NodeT;
typedef NodeT* Tree;

// ########################################################################################################################## //

Tree emptyT();
// PROPÓSITO: Describe un árbol vacío.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

Tree nodeT(int elem, Tree left, Tree right);
// PROPÓSITO: Describe un nodo con el elemento dado y con los subárboles dados.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

bool isEmptyT(Tree t);
// PROPÓSITO: Indica si el árbol dado se encuentra vacío.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

int rootT(Tree t);
// PROPÓSITO: Describe el valor del árbol dado.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

Tree left(Tree t);
// PROPÓSITO: Describe el árbol izquierdo del árbol dado.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

Tree right(Tree t);
// PROPÓSITO: Describe el árbol derecho del árbol dado.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).