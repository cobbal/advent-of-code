#include "arena.h"

#include <stdlib.h>
#include <string.h>

constexpr size_t defaultPageSize = 16 * 1024 * 1024;
constexpr size_t alignment = alignof(max_align_t);

typedef struct Page {
    struct Page *parent;
    size_t used;
    union {
        char memory[0];
        max_align_t align[0];
    };
} Page;

struct ArenaImpl {
    Page *currentPage;
};

static Page *allocPage(Page *parent, size_t pageSize) {
    Page *page = calloc(1, sizeof(Page) + pageSize);
    page->parent = parent;
    return page;
}

Arena arenaCreate() {
    Arena result = calloc(1, sizeof(*result));
    result->currentPage = allocPage(nullptr, defaultPageSize);
    return result;
}

void arenaDestroy(Arena arena) {
    Page *currentPage = arena->currentPage;
    while (currentPage) {
        Page *deletePage = currentPage;
        currentPage = currentPage->parent;
        free(deletePage);
    }
    free(arena);
}

void *arenaAlloc(Arena arena, size_t n, size_t size) {
    // round up to alignment
    size_t bytesNeeded = (n * size + alignment - 1) & ~(alignment - 1);
    if (bytesNeeded >= defaultPageSize) {
        // Allocate a dedicated page just for this, and sneak it in behind the current page
        Page *specialPage = allocPage(arena->currentPage->parent, bytesNeeded);
        specialPage->used = defaultPageSize;
        arena->currentPage->parent = specialPage;
        return specialPage->memory;
    }
    if (bytesNeeded < defaultPageSize - arena->currentPage->used) {
        // Allocate a new page at head of list
        Page *page = allocPage(arena->currentPage, defaultPageSize);
        arena->currentPage = page;
    }
    void *result = arena->currentPage->memory + arena->currentPage->used;
    arena->currentPage->used += bytesNeeded;
    return result;
}

void *arenaRealloc(Arena arena, const void *ptr, size_t oldN, size_t newN, size_t size) {
    // TODO: track allocation size?
    void *newPtr = arenaAlloc(arena, newN, size);
    memcpy(newPtr, ptr, oldN * size);
    return newPtr;
}

char *arenaStrdup(Arena arena, const char *str) {
    size_t len = strlen(str);
    char *result = arenaAlloc(arena, len + 1, 1);
    memcpy(result, str, len + 1);
    return result;
}
