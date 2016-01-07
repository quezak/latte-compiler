#include <stdio.h>
#include <stdlib.h>

void printInt(int n) {
    (void) printf("%d\n", n);
}

void printString(const char* s) {
    (void) printf("%s\n", s);
}

void error() {
    (void) printf("runtime error\n");
    exit(1);
}

int readInt() {
    int n;
    scanf("%d", &n);
    return n;
}

// TODO readString
