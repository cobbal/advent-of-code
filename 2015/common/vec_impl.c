#include "vec_impl.h"

#include <stdlib.h>
#include "common.h"

#define __VEC_IMPL_PASTE(a, b) __VEC_IMPL_PASTE_IMPL(a, b)
#define __VEC_IMPL_PASTE_IMPL(a, b) a##b
#if !defined(VEC_NAME)
#define __VEC_IMPL_DEFAULT_NAME
#define VEC_NAME __VEC_IMPL_PASTE(vec_, VEC_ELEMENT_TYPE)
#endif

VEC_NAME __VEC_IMPL_PASTE(VEC_NAME, _create)(Arena arena) {
    VEC_NAME ret = arenaAlloc(arena, 1, sizeof(*ret));
    ret->arena = arena;
    return ret;
}

VEC_NAME __VEC_IMPL_PASTE(VEC_NAME, _createAndFill)(Arena arena, size_t count, VEC_ELEMENT_TYPE fill) {
    VEC_NAME ret = arenaAlloc(arena, 1, sizeof(*ret));
    ret->arena = arena;
    ret->count = count;
    ret->capacity = count;
    ret->elements = arenaAlloc(arena, count, sizeof(*ret->elements));
    for (size_t i = 0; i < count; i++) {
        ret->elements[i] = fill;
    }
    return ret;
}

void __VEC_IMPL_PASTE(VEC_NAME, _setCapacity)(
    VEC_NAME vec,
    size_t newCapacity
) {
    if (vec->capacity != newCapacity) {
        vec->elements = arenaRealloc(vec->arena, vec->elements, vec->capacity, newCapacity, sizeof(*vec->elements));
        check(vec->elements != nullptr);
        vec->count = min(vec->count, newCapacity);
        vec->capacity = newCapacity;
    }
}

void __VEC_IMPL_PASTE(VEC_NAME, _push)(
    VEC_NAME vec,
    VEC_ELEMENT_TYPE elem
) {
    if (vec->count == vec->capacity) {
        __VEC_IMPL_PASTE(VEC_NAME, _setCapacity)(vec, vec->capacity * 3 / 2 + 16);
    }
    vec->elements[vec->count++] = elem;
}

#undef __VEC_IMPL_PASTE
#undef __VEC_IMPL_PASTE_IMPL
#ifdef __VEC_IMPL_DEFAULT_NAME
#undef VEC_NAME
#undef __VEC_IMPL_DEFAULT_NAME
#endif
