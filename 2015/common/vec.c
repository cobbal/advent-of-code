#include "vec.h"

#include <string.h>
#include "common.h"

typedef struct _vec_impl *gvec;

gvec _vecCreate(Arena arena, size_t elementSize) {
    gvec ret = arenaAlloc(arena, 1, sizeof(*ret));
    ret->arena = arena;
    ret->elementSize = elementSize;
    return ret;
}

gvec _vecCreateAndFill(Arena arena, size_t count, size_t elementSize, const void *fill) {
    gvec ret = arenaAlloc(arena, 1, sizeof(*ret));
    ret->arena = arena;
    ret->count = count;
    ret->capacity = count;
    ret->storage = arenaAlloc(arena, count, elementSize);
    for (size_t i = 0; i < count; i++) {
        memcpy((char *)ret->storage + i * elementSize, fill, elementSize);
    }
    return ret;
}

void _vecSetCapacity(gvec vec, size_t newCapacity) {
    if (vec->capacity != newCapacity) {
        vec->storage = arenaRealloc(vec->arena, vec->storage, vec->capacity, newCapacity, vec->elementSize);
        check(vec->storage != nullptr);
        vec->count = min(vec->count, newCapacity);
        vec->capacity = newCapacity;
    }
}

void _vecPush(gvec vec, const void *elem) {
    if (vec->count == vec->capacity) {
        _vecSetCapacity(vec, vec->capacity * 3 / 2 + 16);
    }
    memcpy((char *)vec->storage + vec->count++ * vec->elementSize, elem, vec->elementSize);
}
