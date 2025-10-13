#include "set.h"
#include "vec_common.h"

#include <stdlib.h>

// TODO: this is the dumbest implementation

struct int64SetImpl_ {
    typeof (*(vec_int64_t){}) vec;
};

int64Set int64SetCreate(Arena arena) {
    return (int64Set)vec_int64_t_create(arena);
}

bool int64SetContains(int64Set set, int64_t element) {
    auto vec = (vec_int64_t)set;
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

void int64SetDump(int64Set set) {
    auto vec = (vec_int64_t)set;
    fprintf(stderr, "{");
    for (size_t i = 0; i < vec->count; i++) {
        fprintf(stderr, "%lld, ", vec->elements[i]);
    }
    fprintf(stderr, "}\n");
}
void int64SetDumpHex(int64Set set) {
    auto vec = (vec_int64_t)set;
    fprintf(stderr, "{");
    for (size_t i = 0; i < vec->count; i++) {
        fprintf(stderr, "%llx, ", vec->elements[i]);
    }
    fprintf(stderr, "}\n");
}
