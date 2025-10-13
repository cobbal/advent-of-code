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
    
    Arena arena = arenaCreate();
    int64_t result0 = part0(arena, f);
    arenaDestroy(arena);
    printf("%20lld ", result0);
    
    fseek(f, 0, SEEK_SET);
    
    arena = arenaCreate();
    int64_t result1 = part1(arena, f);
    arenaDestroy(arena);
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
ssize_t getUntilDelimiter(Arena arena, char **s, ssize_t *n, int delim, FILE *fp) {
    ssize_t pos = 0;
    int c = fgetc(fp);
    while (true) {
        if (pos == *n) {
            ssize_t oldN = *n;
            *n = oldN * 3 / 2 + 16;
            *s = arenaRealloc(arena, *s, oldN, *n, 1);
        }
        if (c == delim || c == EOF) {
            (*s)[pos] = '\0';
            if (c == EOF && pos == 0) {
                return -1;
            }
            return pos;
        }
        (*s)[pos++] = (char) c;
        c = fgetc(fp);
    }
}
