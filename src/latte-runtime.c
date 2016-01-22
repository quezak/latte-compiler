#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
/*
 * Latte runtime library, containing all the required predefined functions, and additional
 * utilities for string concatenation and memory allocation.
 */

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
    // To properly return from readString() after a call to readInt(), consume the possible
    // whitespace following the returned number (but at most one newline, to enable readString
    // to return empty lines).
    char next = getc(stdin);
    while (next == ' ' || next == '\t') next = getc(stdin); // consume horizontal whitespace
    if (next == '\r') next = getc(stdin); // in case of DOS endlines
    if (next != EOF && next != '\n' && next != '\r') ungetc(next, stdin);
    return n;
}

// Return a line from standard input (including empty lines).
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
        (void) printf("FATAL ERROR: can't concatenate: memory allocation error");
        exit(ENOMEM);
    }
    strcpy(dest, a);
    strcpy(dest + lenA, b);
    return dest;
}

void* getMemory(size_t size) {
    void *res = malloc(size);
    if (!res) {
        (void) printf("FATAL ERROR: can't allocate memory for object");
        exit(ENOMEM);
    }
    return res;
}

void freeMemory(void *ptr) {
    free(ptr);
}
