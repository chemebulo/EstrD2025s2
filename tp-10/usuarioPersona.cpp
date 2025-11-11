#include <iostream>
#include "Persona.h"
using namespace std;

/* INTERFAZ DE PERSONA:
    * Persona consPersona(string nombre, int edad)
    * string nombre(Persona p)
    * int edad(Persona p)
    * void crecer(Persona p)
    * void cambioDeNombre(string nombre, Persona p)
    * bool esMayorQueLaOtra(Persona p1, Persona p2)
    * Persona laQueEsMayor(Persona p1, Persona p2)
*/

int main() {
    Persona carlos = consPersona("Carlos", 22);
    Persona jose = consPersona("Jose", 43);
    cout << endl;
    cout << "El nombre de la primera persona es " << nombre(carlos) << "." << endl;
    cout << "El nombre de la segunda persona es " << nombre(jose) << "." << endl;
    cout << endl;
    cout << "La edad de Carlos es " << edad(carlos) << "." << endl;
    cout << "La edad de Jose es " << edad(jose) << "." << endl;
    cout << endl;
    crecer(carlos);
    crecer(carlos);
    crecer(carlos);
    cout << "Despues de que crezca 3 veces, la edad de Carlos es " << edad(carlos) << "." << endl;
    cout << endl;
    cambioDeNombre("Carlitos", carlos);
    cout << "El nombre de la primera persona es " << nombre(carlos) << "." << endl;
    cout << endl;
    cout << boolalpha << "Es mayor Carlitos que Jose: " << esMayorQueLaOtra(carlos, jose) << endl;
    cout << "Quien es mayor entonces, Carlos o Jose: " << laQueEsMayor(carlos, jose)->nombre << endl;
    return 0;
}