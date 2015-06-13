#include <stdio.h>
#include <stdlib.h>

char a;
int b;
double c;

struct P1 {
    int a;
    int b;
    double c;
};

struct P2 {
    int a;
    int b;
    int c;
    int d;
    double e;
};

struct P3 {
    int a;
    int b;
    int c;
    int d;
    double e;
    struct P1 *p1;
    struct P1 *p2;
    struct P1 *p3;
};

void main() {
    struct P1 *p1;
    struct P2 *p2;
    struct P3 *p3;
    printf("%d\n", sizeof(*p1));
    printf("%d\n", sizeof(*p2));
    printf("%d\n", sizeof(*p3));
    printf("%d\n", sizeof(a));
    printf("%d\n", sizeof(b));
    printf("%d\n", sizeof(c));
}
