#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

char* readString() {
    char* lineptr = NULL;
    size_t len = 0;
    ssize_t read = getline(&lineptr, &len, stdin);
    if (read <= 0) {
        if (lineptr) free(lineptr);
        if (read == 0 || read == EOF) return "";
        (void) printf("readString error: %s\n", strerror(read));
        exit(-read);
    }
    // strip the newline character appended by getline
    if (lineptr[read-1] == '\n') lineptr[--read] = 0;
    if (read && lineptr[read-1] == '\r') lineptr[--read] = 0;
    return lineptr;
}

char* concatString(const char* a, const char* b) {
    int lenA = strlen(a);
    int len = lenA + strlen(b);
    char* dest = (char*) malloc(len * sizeof(char));
    if (!dest) {
        (void) printf("can't concatenate: memory allocation error");
        exit(2);
    }
    strcpy(dest, a);
    strcpy(dest + lenA, b);
    return dest;
}
