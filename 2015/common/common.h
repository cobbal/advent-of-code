#pragma once
#include <stdint.h>
#include <stdio.h>
#include "arena.h"
#include "vec.h"

#define check(_Expression) \
(void) \
((!!(_Expression)) || \
 (checkFail(#_Expression,__FILE__,__LINE__),0))

__attribute__((noreturn))
void checkFail(char *expr, char *file, int line);

typedef struct {
    int number;

    int (*dayMain)();

    bool isQuick;
} daySolver;

typedef int64_t (*partSolverInt)(Arena, FILE *);

typedef char *(*partSolverStr)(Arena, FILE *);

[[nodiscard]]
int checkInputInt(char *path, partSolverInt part0, int64_t expected0, partSolverInt part1, int64_t expected1);

[[nodiscard]]
int checkInputStr(char *path, partSolverStr part0, char *expected0, partSolverStr part1, char *expected1);

ssize_t getUntilDelimiter(Arena arena, char **s, ssize_t *n, int delim, FILE *fp);

char *readLine(Arena arena, FILE *fp);

bool readLineWords(Arena arena, FILE *fp, VecString *outWords);

#define min(a,b) ({ __typeof__ (a) _a = (a); __typeof__ (b) _b = (b); _a <= _b ? _a : _b; })
#define max(a,b) ({ __typeof__ (a) _a = (a); __typeof__ (b) _b = (b); _a > _b ? _a : _b; })
