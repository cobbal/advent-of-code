#include "set.h"

#include <stdlib.h>
#include <string.h>
#include "common.h"

// TODO: this is the dumbest implementation

struct int64SetImpl_ {
    VecI64 vec;
};

int64Set int64SetCreate(Arena arena) {
    int64Set set = arenaAlloc(arena, 1, sizeof(*set));
    VEC_INIT(&set->vec, arena);
    return set;
}

bool int64SetContains(int64Set set, int64_t value) {
    VEC_FOR(element, set->vec) {
        if (*element == value) {
            return true;
        }
    }
    return false;
}

bool int64SetInsert(int64Set set, int64_t element) {
    if (int64SetContains(set, element)) {
        return false;
    }
    VEC_PUSH(set->vec, element);
    return true;
}

size_t int64SetCount(int64Set set) {
    return VEC_COUNT(set->vec);
}

void int64SetDump(int64Set set) {
    fprintf(stderr, "{");
    VEC_FOR(element, set->vec) {
        fprintf(stderr, "%lld, ", *element);
    }
    fprintf(stderr, "}\n");
}

void int64SetDumpHex(int64Set set) {
    fprintf(stderr, "{");
    VEC_FOR(element, set->vec) {
        fprintf(stderr, "%llx, ", *element);
    }
    fprintf(stderr, "}\n");
}
