#include <stdio.h>  
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <entradas.h>

int lab1 (void) {
  int x, y, z;

  x = pedir_entero('x');
  y = pedir_entero('y');
  z = pedir_entero('z');

  printf("x + y + 1 = %d \n", x + y + 1);
  printf("z * z + y * 45 - 15 * x = %d \n", z * z + y * 45 - 15 * x);
  printf("y - 2 == (x * 3 + 1) %% 5 = %s \n", (y - 2 == (x * 3 + 1) % 5) ? "true" : "false");
  printf("y / 2 * x = %d \n", y / 2 * x);
  printf("y < x * z = %s \n", (y < x * z) ? "true" : "false");

  return 0;
}

int lab3 (void) {
  int x, y;

  // 1.a.

  x = 5;
  printf("x = %d", x);

  x = pedir_entero('x');
  y = pedir_entero('y');

  // 1.b. 
  x = x + y;
  y = y + y;
  printf("1.b.\n");
  printf("x = %d\n", x);
  printf("y = %d\n", y);

  // 1.c. 
  y = y + y;
  x = x + y;
  printf("1.c.\n");
  printf("x = %d\n", x);
  printf("y = %d\n", y);

  // 4.
  y = y + y;
  x = x + y;
  printf("1.d.\n");
  printf("x = %d\n", x);
  printf("y = %d\n", y);
}

int lab5 (void) {
  int x, y; 

  x = pedir_entero('x');
  y = pedir_entero('y');

  if (x >= y) {
    x = 0;
    printf("Estado 1: x = %d, y = %d", x, y);
  } else if (x <= y) {
    x = 2;
    printf("Estado 2: x = %d, y = %d", x, y);
  }

  printf("Estado 3: x = %d, y = %d", x, y);
}

void lab8a (void) {
  int x = 13;
  int y = 3;
  int i = 0;
  while (x >= y) {
    x = x - y;
    i = i + 1; 
    printf("x: %d, y: %d, i: %d\n", x, y, i);
  }
}

void lab8b (void) {
  int i = 2; 
  int x = 5;
  bool res = true; 

  while (i < x && res) {
    res = res && (x % i != 0);
    i += 1; 
    printf("x: %d, i: %d\n", x, i);
  }
}

void lab9a (void) {
  const int A[4] = { 2, 10, 10, -1 };
  int i = 0;
  int s = 0; 

  while (i < 4) {
    s = s + A[i]; 
    i += 1;
    printf("s: %d, i: %d\n", s, i);
  }
}

int main (void) {
  // lab1();
  // lab3();
  // lab5();
  // lab8a();
  // lab8b();
  // lab9a();
  return 0;
}