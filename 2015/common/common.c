#include "common.h"
#include "vec_common.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
    printf("%20lld ", result0);
    bool part0Correct = result0 == expected0;
    arenaDestroy(arena);

    fseek(f, 0, SEEK_SET);

    arena = arenaCreate();
    int64_t result1 = part1(arena, f);
    printf("%20lld ", result1);
    bool part1Correct = result1 == expected1;
    arenaDestroy(arena);

    check(!fclose(f));
    printf(part0Correct ? "  good" : "   bad");
    printf(part1Correct ? "  good\n" : "   bad\n");
    return !(part0Correct && part1Correct);
}

int checkInputStr(char *path, partSolverStr part0, char *expected0, partSolverStr part1, char *expected1) {
    printf("%22s: ", path);
    FILE *f = fopen(path, "r");
    check(f);

    Arena arena = arenaCreate();
    char *result0 = part0(arena, f);
    printf("%20s ", result0);
    bool part0Correct = strcmp(result0, expected0) == 0;
    arenaDestroy(arena);

    fseek(f, 0, SEEK_SET);

    arena = arenaCreate();
    char *result1 = part1(arena, f);
    printf("%20s ", result1);
    bool part1Correct = strcmp(result1, expected1) == 0;
    arenaDestroy(arena);

    check(!fclose(f));
    printf(part0Correct ? "  good" : "   bad");
    printf(part1Correct ? "  good\n" : "   bad\n");
    return !(part0Correct && part1Correct);
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

char *readLine(Arena arena, FILE *fp) {
    char *line = nullptr;
    ssize_t lineLen = 0;
    if (getUntilDelimiter(arena, &line, &lineLen, '\n', fp) == EOF) {
        return nullptr;
    }
    return line;
}

vec_string readLineWords(Arena arena, FILE *fp) {
    char *line = readLine(arena, fp);
    if (!line) { return nullptr; }
    char *space;
    auto res = vec_string_create(arena);
    while ((space = strchr(line, ' '))) {
        *space = '\0';
        vec_string_push(res, line);
        line = space + 1;
    }
    vec_string_push(res, line);
    return res;
}
