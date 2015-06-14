#include <stdio.h>

int a;
int p;
int t;
int *pp;

a = 1;
p = 0;
t = 0;

while (a < 100)
{
    printf("%d\n", a);
    t = a;
    a = t + p;
    p = t;
}

a = 0;
pp = NULL;
while (!pp) {
    printf("%d\n", a);
    a += 1;
    if (a == 8)
    	pp = &a;
}

p = 10;
pp = &p;
while (pp) {
    printf("%d\n", p);
    p -= 1;
    if (p == 2)
    	pp = NULL;
}

void main() {}
