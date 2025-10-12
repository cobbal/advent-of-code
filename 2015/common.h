#pragma once
#include <stdint.h>
#include <stdio.h>

#define check(b) checkImpl((b), #b, __FILE__, __LINE__)

typedef struct {
    int number;
    int (*dayMain)();
    bool isQuick;
} daySolver;

typedef int64_t (*partSolverInt)(FILE *);

[[nodiscard]]
int checkInputInt(char *path, partSolverInt part0, int64_t expected0, partSolverInt part1, int64_t expected1);

ssize_t getUntilDelimiter(char **s, ssize_t *n, int delim, FILE *fp);

#define min(a,b) ({ __typeof__ (a) _a = (a); __typeof__ (b) _b = (b); _a <= _b ? _a : _b; })
#define max(a,b) ({ __typeof__ (a) _a = (a); __typeof__ (b) _b = (b); _a > _b ? _a : _b; })
