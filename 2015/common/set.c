#include "set.h"

#include <stdlib.h>

#define VEC_ELEMENT_TYPE int64_t
#include "vec_impl.c"
#undef VEC_ELEMENT_TYPE

// TODO: this is the dumbest implementation

struct int64SetImpl_ {
    typeof (*(vec_int64_t){}) vec;
};

int64Set int64SetCreate() {
    return (int64Set)vec_int64_t_create();
}

bool int64SetContains(int64Set set, int64_t element) {
    vec_int64_t vec = (vec_int64_t)set;
    for (size_t i = 0; i < vec->count; i++) {
        if (vec->elements[i] == element) {
            return true;
        }
    }
    return false;
}

bool int64SetInsert(int64Set set, int64_t element) {
    if (int64SetContains(set, element)) {
        return false;
    }
    vec_int64_t_push((vec_int64_t)set, element);
    return true;
}

size_t int64SetCount(int64Set set) {
    return ((vec_int64_t) set)->count;
}

void int64SetDestroy(int64Set set) {
    vec_int64_t_destroy((vec_int64_t) set);
}

void int64SetDump(int64Set set) {
    vec_int64_t vec = (vec_int64_t)set;
    fprintf(stderr, "{");
    for (size_t i = 0; i < vec->count; i++) {
        fprintf(stderr, "%lld, ", vec->elements[i]);
    }
    fprintf(stderr, "}\n");
}
void int64SetDumpHex(int64Set set) {
    vec_int64_t vec = (vec_int64_t)set;
    fprintf(stderr, "{");
    for (size_t i = 0; i < vec->count; i++) {
        fprintf(stderr, "%llx, ", vec->elements[i]);
    }
    fprintf(stderr, "}\n");
}
