#include "common.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

void checkImpl(bool b, char *expr, char *file, int line) {
    if (!b) {
        fprintf(stderr, "\nERROR %s:%d: check failure: %s\n\n", file, line, expr);
        exit(1);
    }
}

int checkInputInt(char *path, partSolverInt part0, int64_t expected0, partSolverInt part1, int64_t expected1) {
    printf("%22s: ", path);
    FILE *f = fopen(path, "r");
    check(f);
    int64_t result0 = part0(f);
    printf("%20lld ", result0);
    fseek(f, 0, SEEK_SET);
    int64_t result1 = part1(f);
    printf("%20lld ", result1);
    check(!fclose(f));
    int result = 0;
    if (result0 == expected0) {
        printf("  good");
    } else {
        printf("   bad");
        result = 1;
    }
    if (result1 == expected1) {
        printf(" good\n");
    } else {
        printf("  bad\n");
        result = 1;
    }
    return result;
}

// https://www.reddit.com/r/C_Programming/comments/m5nzl7/comment/gr19nfn
ssize_t getUntilDelimiter(char **s, ssize_t *n, int delim, FILE *fp) {
    ssize_t pos = 0;
    int c = fgetc(fp);
    if (c == EOF) {
        return -1;
    }
    while (true) {
        if (pos == *n) {
            *s = (char *) realloc(*s, *n = (*n) * 3 / 2 + 16);
        }
        if (c == delim || c == EOF) {
            (*s)[pos] = '\0';
            return pos;
        }
        (*s)[pos++] = (char) c;
        c = fgetc(fp);
    }
}
