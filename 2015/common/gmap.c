#include "gmap.h"

#include <string.h>

#include "common.h"

typedef enum { R, B } Color;

typedef struct Node {
    Color color;
    const struct Node *left;
    GKeyValue kv;
    const struct Node *right;
} Node, *Tree;

typedef const Node *CTree;

static Tree treeCreate(Arena arena, Color color, CTree left, GKeyValue kv, CTree right) {
    Tree newTree = arenaAlloc(arena, 1, sizeof(*newTree));
    *newTree = (Node){color, left, kv, right};
    return newTree;
}

typedef int (*CompareFn)(const void *, const void *);

struct GMap_impl {
    size_t count;
    Arena arena;
    CompareFn cmp;
    CTree tree;
};

static Tree lbalance(Arena arena, Color color, CTree left, GKeyValue kv, CTree right) {
    CTree a, b, c, d;
    GKeyValue x, y, z;
    bool redLeft = color == B && left && left->color == R;
    if (redLeft && left->left && left->left->color == R) {
        a = left->left->left;
        x = left->left->kv;
        b = left->left->right;
        y = left->kv;
        c = left->right;
        z = kv;
        d = right;
    } else if (redLeft && left->right && left->right->color == R) {
        a = left->left;
        x = left->kv;
        b = left->right->left;
        y = left->right->kv;
        c = left->right->right;
        z = kv;
        d = right;
    } else {
        return treeCreate(arena, color, left, kv, right);
    }
    Tree newLeft = treeCreate(arena, B, a, x, b);
    Tree newRight = treeCreate(arena, B, c, z, d);
    return treeCreate(arena, R, newLeft, y, newRight);
}

static Tree rbalance(Arena arena, Color color, CTree left, GKeyValue kv, CTree right) {
    CTree a, b, c, d;
    GKeyValue x, y, z;
    bool redRight = color == B && right && right->color == R;
    if (redRight && right->left && right->left->color == R) {
        a = left;
        x = kv;
        b = right->left->left;
        y = right->left->kv;
        c = right->left->right;
        z = right->kv;
        d = right->right;
    } else if (redRight && right->right && right->right->color == R) {
        a = left;
        x = kv;
        b = right->left;
        y = right->kv;
        c = right->right->left;
        z = right->right->kv;
        d = right->right->right;
    } else {
        return treeCreate(arena, color, left, kv, right);
    }
    Tree newLeft = treeCreate(arena, B, a, x, b);
    Tree newRight = treeCreate(arena, B, c, z, d);
    return treeCreate(arena, R, newLeft, y, newRight);
}


static struct InsertHelperResult {
    Tree tree;
    bool replaced;
    void *replacedValue;
} insertHelper(Arena arena, const Node *tree, CompareFn cmp, GKeyValue kv) {
    if (!tree) {
        return (struct InsertHelperResult){
            treeCreate(arena, R, nullptr, kv, nullptr),
            false,
            nullptr
        };
    }
    int order = cmp(kv.key, tree->kv.key);
    if (order < 0) {
        auto newLeft = insertHelper(arena, tree->left, cmp, kv);
        newLeft.tree = lbalance(
            arena, tree->color,
            newLeft.tree,
            tree->kv,
            tree->right
        );
        return newLeft;
    } else if (order > 0) {
        auto newRight = insertHelper(arena, tree->right, cmp, kv);
        newRight.tree = rbalance(
            arena, tree->color,
            tree->left,
            tree->kv,
            newRight.tree
        );
        return newRight;
    } else {
        return (struct InsertHelperResult){
            treeCreate(arena, tree->color, tree->left, kv, tree->right),
            true,
            tree->kv.value
        };
    }
}

static GMap gmapCreateFill(size_t count, Arena arena, CompareFn cmp, CTree tree) {
    GMap map = arenaAlloc(arena, sizeof(*map), 1);
    *map = (struct GMap_impl){count, arena, cmp, tree};
    return map;
}

GMap gmapEmpty(Arena arena, int (*cmp)(const void *, const void *)) {
    return gmapCreateFill(0, arena, cmp, nullptr);
}

size_t gmapCount(GMap map) {
    return map->count;
}

struct GMapLookupResult gmapLookup(GMap map, const void *key) {
    CTree tree = map->tree;
    while (tree) {
        int order = map->cmp(key, tree->kv.key);
        if (order < 0) {
            tree = tree->left;
        } else if (order > 0) {
            tree = tree->right;
        } else {
            return (struct GMapLookupResult){true, tree->kv.value};
        }
    }
    return (struct GMapLookupResult){false, nullptr};
}

struct GMapInsertResult gmapInsert(GMap map, const void *key, void *value) {
    auto subResult = insertHelper(map->arena, map->tree, map->cmp, (GKeyValue){key, value});
    subResult.tree->color = B;
    return (struct GMapInsertResult){
        gmapCreateFill(map->count + !subResult.replaced, map->arena, map->cmp, subResult.tree),
        subResult.replaced,
        subResult.replacedValue
    };
}

// GMap gmapPop(GMap map, const void **outKey, void **outValue) {
//     check(false);
// }

static void fillElements(CTree tree, GKeyValue **destination, size_t *space) {
    if (!tree) { return; }
    fillElements(tree->left, destination, space);
    check(*space > 0);
    *((*destination)++) = tree->kv;
    (*space)--;
    fillElements(tree->right, destination, space);
}

GKeyValue *gmapElements(GMap map) {
    size_t space = map->count;
    GKeyValue *result = arenaAlloc(map->arena, space, sizeof(*result));
    GKeyValue *fill = result;
    fillElements(map->tree, &fill, &space);
    check(space == 0);
    return result;
}
