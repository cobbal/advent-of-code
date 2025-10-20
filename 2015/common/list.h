#pragma once

#include "arena.h"

struct _list_impl {
    void *_storage;
};

#define LIST(type) union { \
    struct _list_impl _list; \
    type *_payload; \
}

struct _list_impl _listCons(Arena arena, size_t elementSize, const void *carPtr, struct _list_impl cdr);
struct _list_impl _listGetCdr(struct _list_impl list, size_t elementSize);

#define LIST_NIL(listType) ((listType){ ._list = { ._storage = nullptr } })
#define LIST_CONS(arena, car, cdr) ({ __typeof(*(cdr)._payload) _car = (car); (__typeof(cdr)){ ._list = _listCons(arena, sizeof(*(cdr)._payload), &_car, (cdr)._list) };})
#define LIST_IS_NIL(list) (!(list._list._storage))
#define LIST_CAR(list) (*(__typeof((list)._payload))((list)._list._storage))
#define LIST_CDR(list) (__typeof(list)){ ._list = _listGetCdr((list)._list, sizeof(*(list)._payload)) }

// #define VEC_FOR(var, list) for (auto var = VEC_ELEMS(list); var < VEC_ELEMS(list) + VEC_COUNT(list); var++)
// #define VEC_FORI(var, list) for (\
//     struct { size_t i; __typeof((list)._payload) ptr; } var = { 0, VEC_ELEMS(list) }; \
//     var.i < VEC_COUNT(list); \
//     var.i++, var.ptr++ \
// )
// #define VEC_OF_PTR(listtype, ptr) ((listtype){ ._list = ptr })
// #define PTR_OF_VEC(ptr) ((void *)((ptr)._list))
// #define VEC_DEBUG(var, list, format, ...) ({ \
//     fprintf(stderr, "["); \
//     VEC_FOR(var, list) { \
//         if (var != VEC_ELEMS(list)) { fprintf(stderr, ", "); } \
//         fprintf(stderr, format, __VA_ARGS__); \
//     } \
//     fprintf(stderr, "]\n"); \
// })
//
//
// typedef VEC(char) VecChar;
//
// typedef VEC(char *) VecString;
//
// typedef VEC(int64_t) VecI64;
//
// typedef VEC(intptr_t) VecIntptr;
//
// typedef VEC(VecIntptr) VecVecIntptr;
