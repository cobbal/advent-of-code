#pragma once
#include "arena.h"
#include "vec.h"

typedef struct GMap_impl *GMap;

typedef VEC(GMap) VecGMap;

typedef struct {
    const void *key;
    void *value;
} GKeyValue;

typedef VEC(GKeyValue) VecGKeyValue;

GMap gmapEmpty(Arena arena, int (*cmp)(const void *, const void *));

size_t gmapCount(GMap map);

struct GMapLookupResult {
    bool found;
    void *value;
} gmapLookup(GMap map, const void *key);

[[nodiscard]]
struct GMapInsertResult {
    GMap map;
    bool replaced;
    void *replacedValue;
} gmapInsert(GMap map, const void *key, void *value);

// [[nodiscard]]
// GMap gmapRemove(GMap map, const void *key);

// [[nodiscard]]
// GMap gmapPop(GMap map, const void **outKey, void **outValue);

VecGKeyValue gmapElements(GMap map);
