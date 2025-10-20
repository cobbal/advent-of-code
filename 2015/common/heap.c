#include "heap.h"

#include "common/common.h"

GHeap gheapCreate(Arena arena, int (*cmp)(const void *, const void *)) {
    GHeap heap;
    heap._cmp = cmp;
    VEC_INIT(&heap._keys, arena);
    VEC_INIT(&heap._values, arena);
    return heap;
}

static void heapify(GHeap heap, size_t i) {
    size_t n = VEC_COUNT(heap._keys);
    void **keys = VEC_ELEMS(heap._keys);
    void **values = VEC_ELEMS(heap._values);
    while (true) {
        size_t l = 2 * i + 1;
        size_t r = 2 * i + 2;
        size_t smallest = i;
        if (l < n && heap._cmp(keys[l], keys[i]) < 0) {
            smallest = l;
        }
        if (r < n && heap._cmp(keys[r], keys[smallest]) < 0) {
            smallest = r;
        }
        if (smallest == i) { return; }
        void *tmpK = keys[i];
        void *tmpV = values[i];
        keys[i] = keys[smallest];
        values[i] = values[smallest];
        keys[smallest] = tmpK;
        values[smallest] = tmpV;
        i = smallest;
    }
}

GKeyValue gheapPopMin(GHeap heap) {
    GKeyValue result = {.key = VEC_ELEMS(heap._keys)[0], .value = VEC_ELEMS(heap._values)[0]};
    size_t length = VEC_COUNT(heap._keys);
    VEC_ELEMS(heap._keys)[0] = VEC_ELEMS(heap._keys)[length - 1];
    VEC_ELEMS(heap._values)[0] = VEC_ELEMS(heap._values)[length - 1];
    VEC_RESIZE(heap._keys, length - 1);
    VEC_RESIZE(heap._values, length - 1);
    heapify(heap, 0);
    return result;
}

void gheapInsert(GHeap heap, void *key, void *value) {
    VEC_PUSH(heap._keys, nullptr);
    VEC_PUSH(heap._values, nullptr);
    void **keys = VEC_ELEMS(heap._keys);
    void **values = VEC_ELEMS(heap._values);
    size_t i = VEC_COUNT(heap._keys) - 1;
    for (; i > 0 && heap._cmp(key, keys[(i - 1) / 2]) < 0; i = (i - 1) / 2) {
        keys[i] = keys[(i - 1) / 2];
        values[i] = values[(i - 1) / 2];
    }
    keys[i] = key;
    values[i] = value;
}

size_t gheapCount(GHeap heap) { return VEC_COUNT(heap._keys); }

GKeyValue gheapPeekMin(GHeap heap) {
    return (GKeyValue){.key = VEC_ELEMS(heap._keys)[0], .value = VEC_ELEMS(heap._values)[0]};
}
