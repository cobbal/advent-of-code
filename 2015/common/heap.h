#pragma once

#include "gmap.h"
#include "vec.h"

typedef struct {
    int (*_cmp)(const void *, const void *);
    VecVoidptr _keys;
    VecVoidptr _values;
} GHeap;

GHeap gheapCreate(Arena arena, int (*cmp)(const void *, const void *));
size_t gheapCount(GHeap heap);
GKeyValue gheapPeekMin(GHeap heap);
GKeyValue gheapPopMin(GHeap heap);
void gheapInsert(GHeap heap, void *key, void *value);
