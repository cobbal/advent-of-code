#pragma once
#include <stdint.h>
#include <stdio.h>

typedef struct int64SetImpl_ *int64Set;
extern int64Set int64SetCreate();
extern bool int64SetContains(int64Set set, int64_t element);
extern bool int64SetInsert(int64Set set, int64_t element);
extern size_t int64SetCount(int64Set set);
extern void int64SetDestroy(int64Set set);

extern void int64SetDump(int64Set set);
extern void int64SetDumpHex(int64Set set);
