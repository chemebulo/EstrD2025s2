#include <iostream>
using namespace std;

// EJERCICIO 1:

    // https://prnt.sc/-Bx38ROVy9Mo


// EJERCICIO 2:

void printFromTo(char c1, char c2) {
// PROPÓSITO: Imprime los caracteres desde c1 hasta c2 en orden ASCII, separados por comas.
// PRECONDICIÓN: c1 < c2
    for(int i = 0; c1 + i <= c2; i++) {
        cout << c1 + i << ", ";
    }
    cout << endl;
}

int fc(int n) {
// PROPÓSITO: Calcula y devuelve el factorial de n.
// PRECONDICIÓN: n >= 0
    int x = 1;
    while(n > 0) {
        x = x * n;
        n--;
    }
    return x;
}

int ft(int n, int m) {
// PROPÓSITO: Calcula y devuelve la suma de los enteros desde n hasta m.
// PRECONDICIÓN: n <= m
    if (n == m) {
        return n;
    }
    return n + ft(n+1, m);
}


// EJERCICIO 3:

    // Hecho.


// EJERCICIO 4:

void printN(int n, string s){
// PROPÓSITO: Imprime n veces un string s.
    for(int i = 0; i < n; i++){
        cout << s << endl;
    }
}

void printNV2(int n, string s){
// PROPÓSITO: Imprime n veces un string s.
    if(n <= 0) return;
    cout << s << endl;
    printNV2(n - 1, s);
}


void cuentaRegresiva(int n){
// PROPÓSITO: Imprime los números desde n hasta 0, separados por saltos de línea.
    for (int i = n; i >= 0; i--) {
        cout << i << endl;
    }
}

void cuentaRegresivaV2(int n){
// PROPÓSITO: Imprime los números desde n hasta 0, separados por saltos de línea.
    if(n < 0) return;
    cout << n << endl;
    cuentaRegresivaV2(n - 1);
}


void desdeCeroHastaN(int n){
// PROPÓSITO: Imprime los números de 0 hasta n, separados p or saltos de línea.
    for(int i = 0; i <= n; i++) {
        cout << i << endl;
    }
}

void desdeCeroHastaNV2(int n){
// PROPÓSITO: Imprime los números de 0 hasta n, separados p or saltos de línea.
    if(n < 0) return;
    desdeCeroHastaNV2(n - 1);
    cout << n << endl;
}


int valorAbsoluto(int n) {
// PROPÓSITO: Devuelve el valor absoluto del número dado.
    if(n < 0) { return -n; } 
         else { return n;  }
}

int signoDelProducto(int n, int m) {
// PROPÓSITO: Devuelve el signo del producto (+1 o -1).
    if ((n < 0) ^ (m < 0)) { return -1; } 
                      else { return 1;  }
}

int multiplicarPorIteracion(int n, int m) {
// PROPÓSITO: Multiplica dos enteros positivos con sumas repetidas.
    int resultado = 0;
    for (int i = 0; i < m; i++) {
        resultado += n;
    }
    return resultado;
}

int mult(int n, int m){
// PROPÓSITO: Realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
    int absN = valorAbsoluto(n);
    int absM = valorAbsoluto(m);
    int signo = signoDelProducto(n, m);
    int resultado = multiplicarPorIteracion(absN, absM);
    return signo * resultado;
}

int multiplicarPorRecursion(int n, int m) {
// PROPÓSITO: Multiplica dos enteros positivos con sumas.
    if(m == 0) return 0;
    return n + multiplicarPorRecursion (n, m - 1);
}

int multV2(int n, int m){
// PROPÓSITO: Realiza la multiplicación entre dos números (sin utilizar la op eración * de C++).
    int absN = valorAbsoluto(n);
    int absM = valorAbsoluto(m);
    int signo = signoDelProducto(n, m);
    int resultado = multiplicarPorRecursion(absN, absM);
    return signo * resultado;
}


void primerosN(int n, string s){
// PROPÓSITO: Imprime los primeros n char del string s, separados p or un salto de línea.
// PRECONDICIÓN: El string tiene al menos n char.
    for(int i = 0; i < n && i < s.size(); i++) {
        cout << s[i] << endl;
    }
}

void primerosNV2(int n, string s){
// PROPÓSITO: Imprime los primeros n char del string s, separados p or un salto de línea.
// PRECONDICIÓN: El string tiene al menos n char.
    if (n <= 0) return;
    cout << s[s.size() - n] << endl;
    primerosNV2(n - 1, s);
}


bool pertenece(char c, string s){
// PROPÓSITO: Indica si un char c aparece en el string s.
    int i = 0;
    while(i < s.size() && not (s[i] == c)) {
        i++;
    }
    return s[i] == c;
}

bool perteneceV2(char c, string s){
// PROPÓSITO: Indica si un char c aparece en el string s.
    if (s.empty()) {
        return false;
    }

    if (s[0] == c) {
        return true;
    }

    return pertenece(c, s.substr(1));
}


int apariciones(char c, string s){
// PROPÓSITO: Devuelve la cantidad de apariciones de un char c en el string s.
    int resultado = 0;
    for(int i = 0; i < s.size(); i++) {
        resultado += (c == s[i]);
    }
    return resultado;
}

int aparicionesV2(char c, string s){
// PROPÓSITO: Devuelve la cantidad de apariciones de un char c en el string s.
    if (s.empty()) {
        return 0;
    }

    int resultado = (s[0] == c) ? 1 : 0;

    return resultado + aparicionesV2(c, s.substr(1));
}


// EJERCICIO 5:

    // Hecho.