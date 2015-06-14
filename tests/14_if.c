#include <stdio.h>

int a = 1;

if (a)
    printf("a is true\n");
else
    printf("a is false\n");

int b = 0;
if (b)
    printf("b is true\n");
else
    printf("b is false\n");

int *c = 0;
if (c)
    printf("c is true\n");
else
    printf("c is false\n");
if (!c)
    printf("c is true\n");
else
    printf("c is false\n");
c = &b;
if (c)
    printf("c is true\n");
else
    printf("c is false\n");
if (!c)
    printf("c is true\n");
else
    printf("c is false\n");

void main() {}
