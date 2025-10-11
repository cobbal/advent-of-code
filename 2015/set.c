#include "set.h"

#include <stdlib.h>

// TODO: this is the dumbest implementation

struct int64SetImpl_ {
    size_t count;
    size_t capacity;
    int64_t *storage;
};

int64Set int64SetCreate() {
    int64Set set = calloc(1, sizeof(*set));
    set->count = 0;
    set->capacity = 8;
    set->storage = calloc(set->capacity, sizeof(*set->storage));
    return set;
}

bool int64SetContains(int64Set set, int64_t element) {
    for (size_t i = 0; i < set->count; i++) {
        if (set->storage[i] == element) {
            return true;
        }
    }
    return false;
}

bool int64SetInsert(int64Set set, int64_t element) {
    if (int64SetContains(set, element)) {
        return false;
    }
    if (set->count >= set->capacity) {
        set->capacity *= 2;
        set->storage = realloc(set->storage, set->capacity * sizeof(*set->storage));
    }
    set->storage[set->count++] = element;
    return true;
}

size_t int64SetCount(int64Set set) {
    return set->count;
}

void int64SetDestroy(int64Set set) {
    free(set->storage);
    free(set);
}

void int64SetDump(int64Set set) {
    fprintf(stderr, "{");
    for (size_t i = 0; i < set->count; i++) {
        fprintf(stderr, "%lld, ", set->storage[i]);
    }
    fprintf(stderr, "}\n");
}
void int64SetDumpHex(int64Set set) {
    fprintf(stderr, "{");
    for (size_t i = 0; i < set->count; i++) {
        fprintf(stderr, "%llx, ", set->storage[i]);
    }
    fprintf(stderr, "}\n");
}
