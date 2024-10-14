#include <stdio.h>
#include "entradas.h"

int pedir_entero(char n) {
  int x;
  printf("Ingrese un número para %c", n);
  scanf("%d", x);
  return x;
}

void imprimir_entero(int x, char n) {
  printf("El valor de %c es %d", n, x);
}