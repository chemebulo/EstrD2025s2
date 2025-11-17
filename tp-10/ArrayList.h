#include <iostream>
using namespace std;

struct ArrayListSt {
    /* INVARIANTES DE REPRESENTACIÓN.
        - Siendo cd es cp:
            -  "cd" no puede ser mayor a "cp".
            - la capacidad de "es" debe ser igual a "cp".
    */
    int cantidad;       // Cantidad de elementos
    int* elementos;     // Array de elementos
    int capacidad;      // Tamaño del array
};

typedef ArrayListSt* ArrayList;

ArrayList newArrayList();
// PROPÓSITO: Crea una lista con 0 elementos.
// NOTA: Empezar el ArrayList con capacidad 16.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

ArrayList newArrayListWith(int capacidad);
// PROPÓSITO: Crea una lista con 0 elementos y una capacidad dada por parámetro.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

int lengthAL(ArrayList xs);
// PROPÓSITO: Devuelve la cantidad de elementos existentes.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

int get(int i, ArrayList xs);
// PROPÓSITO: Devuelve el iésimo elemento de la lista.
// PRECONDICIÓN: El ArrayList dado debe tener al menos i elementos.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

void set(int i, int x, ArrayList xs);
// PROPÓSITO: Reemplaza el iésimo elemento por otro dado.
// PRECONDICIÓN: El ArrayList dado debe tener al menos i elementos.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).

void resize(int capacidad, ArrayList xs);
// PROPÓSITO: Decrementa o aumenta la capacidad del array.
// NOTA: En caso de decrementarla, se pierden los elementos del final de la lista.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).

void add(int x, ArrayList xs);
// PROPÓSITO: Agrega un elemento al final de la lista.
// COSTO OPERACIONAL: O(1) Amortizado.
// COSTO MEMORIA: O(1).

void remove(ArrayList xs);
// PROPÓSITO: Borra el último elemento de la lista.
// PRECONDICIÓN: El ArrayList no debe estar vacío.
// COSTO OPERACIONAL: O(1).
// COSTO MEMORIA: O(1).