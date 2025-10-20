#include "list.h"

#include <string.h>

static constexpr size_t ptrAlign = alignof(void *);

struct _list_impl _listCons(Arena arena, size_t elementSize, const void *carPtr, struct _list_impl cdr) {
    size_t ptrOffset = (elementSize + ptrAlign - 1) & ~(ptrAlign - 1);
    char *storage = arenaAlloc(arena, 1, ptrOffset + sizeof(void *));
    memcpy(storage, carPtr, elementSize);
    *(void **)(storage + ptrOffset) = cdr._storage;
    return (struct _list_impl){._storage = storage};
}

struct _list_impl _listGetCdr(struct _list_impl list, size_t elementSize) {
    size_t ptrOffset = (elementSize + ptrAlign - 1) & ~(ptrAlign - 1);
    return (struct _list_impl){ ._storage = *(void **)((char *)list._storage + ptrOffset) };
}
