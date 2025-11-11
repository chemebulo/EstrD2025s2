#include <iostream>
#include "Pokemon.h"
using namespace std;

Pokemon consPokemon(TipoDePokemon tipo){
// PROPÓSITO: Dado un tipo devuelve un pokémon con 100 % de energía.
    Pokemon p = new PokeSt;
    p->tipo = tipo;
    p->energia = 100;
    return p;
}

TipoDePokemon tipoDePokemon(Pokemon p){
// PROPÓSITO: Devuelve el tipo de un pokémon.
    return p->tipo;
}

int energia(Pokemon p){
// PROPÓSITO: Devuelve el porcentaje de energía.
    return p->energia;
}

void perderEnergia(int energia, Pokemon p){
// PROPÓSITO: Le resta energía al pokémon.
    if(p->energia - energia >= 0){
        p->energia -= energia;
    } else {
        p->energia = 0;
    }
}

bool esAgua(Pokemon p){
    return p->tipo == "Agua";
}

bool esFuego(Pokemon p){
    return p->tipo == "Fuego";
}

bool esPlanta(Pokemon p){
    return p->tipo == "Planta";
}

bool superaA(Pokemon p1, Pokemon p2){
// PROPÓSITO: Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo.
//            Agua supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
    return (esAgua(p1) && esFuego(p2)) || (esFuego(p1) && esPlanta(p2)) || (esPlanta(p1) && esAgua(p2)); 
}