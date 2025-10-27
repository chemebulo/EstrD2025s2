#include <iostream>
#include "Par.h"
using namespace std;

Par consPar(int n, int m){
// PROPÓSITO: Construye un par.
    Par p;
    p.x = n;
    p.y = m;
    return p;
}

int fst(Par p){
// PROPÓSITO: Devuelve la primera componente.
    return p.x;
}

int snd(Par p){
// PROPÓSITO: Devuelve la segunda componente.
    return p.y;
}

int maxDelPar(Par p){
// PROPÓSITO: Devuelve la mayor componente.
    return p.x > p.y ? p.x : p.y; 
}

Par swap(Par p){
// PROPÓSITO: Devuelve un par con las componentes intercambiadas.
    Par pp;
    pp.x = p.y;
    pp.y = p.x;
    return pp;
}

Par divisionYResto(int n, int m){
// PROPÓSITO: Devuelve un par donde la primer componente es la división y la segunda el resto entre ambos números.
    Par p;
    p.x = n / m;
    p.y = n % m;
    return p;
}