#include <iostream>
#include "ArrayList.h"
using namespace std;

int sumatoria(ArrayList xs){
// PROPÓSITO: Devuelve la suma de todos los elementos.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    int longitud = lengthAL(xs);
    int resultado = 0;

    for(int i = 0; i < longitud; i++){
        resultado += get(i, xs);
    }

    return resultado;
}

void sucesores(ArrayList xs){
// PROPÓSITO: Incrementa en uno todos los elementos.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    int longitud = lengthAL(xs);

    for(int i = 0; i < longitud; i++){
        set(i, (get(i, xs) + 1), xs);
    }
}

bool pertenece(int x, ArrayList xs){
// PROPÓSITO: Indica si el elemento pertenece a la lista.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    int longitud = lengthAL(xs);
    int i = 0;

    while(i < longitud && get(i, xs) != x){
        i++;
    }

    return i < longitud;
}

int apariciones(int x, ArrayList xs){
// PROPÓSITO: Indica la cantidad de elementos iguales a x.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    int longitud = lengthAL(xs);
    int apariciones = 0;

    for(int i = 0; i < longitud; i++){
        if(get(i, xs) == x){
            apariciones++;
        }
    }

    return apariciones;
}

ArrayList append(ArrayList xs, ArrayList ys){
// PROPÓSITO: Crea una nueva lista a partir de la primera y la segunda (en ese orden).
// COSTO OPERACIONAL: O(N + M).
// COSTO MEMORIA: O(1).
    ArrayList ws = newArrayListWith(xs->capacidad + ys->capacidad);

    for(int i = 0; i < xs->cantidad; i++){
        add(get(i, xs), ws);
    }

    for(int i = 0; i < ys->cantidad; i++){
        add(get(i, ys), ws);
    }

    return ws;
}

int minimo(ArrayList xs){
// PROPÓSITO: Devuelve el elemento más chico de la lista.
// PRECONDICIÓN: El ArrayList no debe estar vacío.
// COSTO OPERACIONAL: O(N).
// COSTO MEMORIA: O(1).
    int longitud = lengthAL(xs);
    
    if(longitud == 0){
        throw new runtime_error("No se puede obtener el minimo de un ArrayList vacío.");
    }

    int minimo = get(0, xs);
    int actual;

    for(int i = 1; i < longitud; i++){
        actual = get(i, xs); 
        minimo = actual < minimo ? actual : minimo; 
    }

    return minimo;
}

/* INTERFAZ DE ARRAYLIST:
    * ArrayList newArrayList()
    * ArrayList newArrayListWith(int capacidad)
    * int lengthAL(ArrayList xs)
    * int get(int i, ArrayList xs)
    * void set(int i, int x, ArrayList xs)
    * void resize(int capacidad, ArrayList xs)
    * void add(int x, ArrayList xs)
    * void remove(ArrayList xs)
*/

int main(){
    ArrayList xs = newArrayListWith(3);
    ArrayList ys = newArrayListWith(2);
    cout << "La longitud de xs es: " << lengthAL(xs) << endl;
    cout << "La longitud de ys es: " << lengthAL(ys) << endl;
    add(1, xs);
    add(2, xs);
    add(3, xs);
    add(10, ys);
    add(20, ys);
    cout << endl;
    cout << "Despues de agregar elementos a ambos ArrayLists..." << endl;
    cout << endl;
    cout << "La longitud de xs es: " << lengthAL(xs) << endl;
    cout << "La longitud de ys es: " << lengthAL(ys) << endl;
    remove(xs);
    cout << endl;
    cout << "Despues de eliminar el ultimo elemento en xs..." << endl;
    cout << endl;
    cout << "La longitud de xs es: " << lengthAL(xs) << endl;
    cout << "La longitud de ys es: " << lengthAL(ys) << endl;
    cout << endl;
    cout << "Los elementos actuales de xs son..." << endl;
    cout << endl;

    for(int i = 0; i < lengthAL(xs); i++){
        cout << get(i, xs) << endl;
    }

    cout << endl;
    cout << "Los elementos actuales de ys son..." << endl;
    cout << endl;

    for(int i = 0; i < lengthAL(ys); i++){
        cout << get(i, ys) << endl;
    }

    cout << endl;
    cout << "Despues de hacer un resize de 5 en ys, los elementos actuales son..." << endl;
    cout << endl;
    resize(5, ys);
    add(30, ys);
    add(40, ys);
    add(50, ys);

    for(int i = 0; i < lengthAL(ys); i++){
        cout << get(i, ys) << endl;
    }

    cout << endl;
    cout << "La sumatoria de todo xs es: " << sumatoria(xs) << endl;
    cout << "La sumatoria de todo ys es: " << sumatoria(ys) << endl;
    sucesores(xs);
    sucesores(ys);
    cout << endl;
    cout << "Despues de sumarle uno a todos en xs e ys..." << endl;
    cout << endl;
    cout << "La sumatoria de todo xs es: " << sumatoria(xs) << endl;
    cout << "La sumatoria de todo ys es: " << sumatoria(ys) << endl;
    cout << endl;
    cout << "El minimo de xs es: " << minimo(xs) << endl;
    cout << "El minimo de ys es: " << minimo(ys) << endl;
    return 0;
}