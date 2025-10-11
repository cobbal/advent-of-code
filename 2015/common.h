#pragma once
#include <stdint.h>
#include <stdio.h>

#define check(b) checkImpl((b), #b, __FILE__, __LINE__)

typedef struct {
    int number;

    void (*dayMain)();
} daySolver;

typedef int64_t (*partSolverInt)(FILE *);

void dayHeader(int dayNumber);
void checkInputInt(char *path, partSolverInt part0, int64_t expected0, partSolverInt part1, int64_t expected1);

#define min(a,b) ({ __typeof__ (a) _a = (a); __typeof__ (b) _b = (b); _a <= _b ? _a : _b; })
#define max(a,b) ({ __typeof__ (a) _a = (a); __typeof__ (b) _b = (b); _a > _b ? _a : _b; })
