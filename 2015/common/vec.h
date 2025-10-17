#pragma once

#include <stdint.h>

#include "arena.h"

struct _vec_impl {
    Arena arena;
    size_t count;
    size_t capacity;
    size_t elementSize;
    void *storage;
};

#define VEC(type) union { \
    struct _vec_impl *_vec; \
    type *_payload; \
}

struct _vec_impl *_vecCreate(Arena arena, size_t elementSize);
struct _vec_impl *_vecCreateAndFill(Arena arena, size_t count, size_t elementSize, const void *fill);
void _vecSetCapacity(struct _vec_impl *vec, size_t newCapacity);
void _vecPush(struct _vec_impl *vec, const void *elem);

#define VEC_INIT(vec, arena) ((vec)->_vec = _vecCreate((arena), sizeof(*(vec)->_payload)), (void)0)
#define VEC_INIT_AND_FILL(vec, arena, count, fill) ({ \
    __typeof(*(vec)->_payload) _fill = (fill); \
    (vec)->_vec = _vecCreateAndFill((arena), (count), sizeof(_fill), &_fill); \
})
#define VEC_SET_CAPACITY(vec, newCapacity) _vecSetCapacity((vec)._vec, (newCapacity))
#define VEC_PUSH(vec, elem) ({ __typeof(*(vec)._payload) _elem = (elem); _vecPush((vec)._vec, &_elem); })

#define VEC_ARENA(vec) ((vec)._vec->arena)
#define VEC_COUNT(vec) ((vec)._vec->count)
#define VEC_CAPCITY(vec) ((vec)._vec->capacity)
#define VEC_ELEMS(vec) ((__typeof((vec)._payload))((vec)._vec->storage))
#define VEC_FOR(var, vec) for (auto var = VEC_ELEMS(vec); var < VEC_ELEMS(vec) + VEC_COUNT(vec); var++)

typedef VEC(char) VecChar;
typedef VEC(char *) VecString;
typedef VEC(int64_t) VecI64;
