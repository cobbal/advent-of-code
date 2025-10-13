#include <stdlib.h>
#include "common.h"

#if ! defined(VEC_ELEMENT_TYPE)
#error "must define VEC_ELEMENT_TYPE before including this file"

// define it so that IDE has a better day
#define VEC_ELEMENT_TYPE double
#endif

#define __VEC_IMPL_PASTE(a, b) __VEC_IMPL_PASTE_IMPL(a, b)
#define __VEC_IMPL_PASTE_IMPL(a, b) a##b
#define __VEC_IMPL_TYPE __VEC_IMPL_PASTE(vec_, VEC_ELEMENT_TYPE)

typedef struct {
    Arena arena;
    size_t count;
    size_t capacity;
    VEC_ELEMENT_TYPE *elements;
} *__VEC_IMPL_TYPE;

__VEC_IMPL_TYPE __VEC_IMPL_PASTE(__VEC_IMPL_TYPE, _create)(Arena arena) {
    __VEC_IMPL_TYPE ret = arenaAlloc(arena, 1, sizeof(*ret));
    ret->arena = arena;
    return ret;
}

__VEC_IMPL_TYPE __VEC_IMPL_PASTE(__VEC_IMPL_TYPE, _createAndFill)(Arena arena, size_t count, VEC_ELEMENT_TYPE fill) {
    __VEC_IMPL_TYPE ret = arenaAlloc(arena, 1, sizeof(*ret));
    ret->arena = arena;
    ret->count = count;
    ret->capacity = count;
    ret->elements = arenaAlloc(arena, count, sizeof(*ret->elements));
    for (size_t i = 0; i < count; i++) {
        ret->elements[i] = fill;
    }
    return ret;
}

void __VEC_IMPL_PASTE(__VEC_IMPL_TYPE, _setCapacity)(
    __VEC_IMPL_TYPE vec,
    size_t newCapacity
) {
    if (vec->capacity != newCapacity) {
        vec->elements = arenaRealloc(vec->arena, vec->elements, vec->capacity, newCapacity, sizeof(*vec->elements));
        check(vec->elements != nullptr);
        vec->count = min(vec->count, newCapacity);
        vec->capacity = newCapacity;
    }
}

void __VEC_IMPL_PASTE(__VEC_IMPL_TYPE, _push)(
    __VEC_IMPL_TYPE vec,
    VEC_ELEMENT_TYPE elem
) {
    if (vec->count == vec->capacity) {
        __VEC_IMPL_PASTE(__VEC_IMPL_TYPE, _setCapacity)(vec, vec->capacity * 3 / 2 + 16);
    }
    vec->elements[vec->count++] = elem;
}

#undef __VEC_IMPL_PASTE
#undef __VEC_IMPL_PASTE_IMPL
#undef __VEC_IMPL_TYPE
