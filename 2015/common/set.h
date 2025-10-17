#pragma once
#include <stdint.h>
#include <stdio.h>

#include "arena.h"

typedef struct int64SetImpl_ *int64Set;
extern int64Set int64SetCreate(Arena arena);
extern bool int64SetContains(int64Set set, int64_t value);
extern bool int64SetInsert(int64Set set, int64_t element);
extern size_t int64SetCount(int64Set set);

extern void int64SetDump(int64Set set);
extern void int64SetDumpHex(int64Set set);
