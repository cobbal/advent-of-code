#pragma once
#include <stddef.h>

typedef struct ArenaImpl *Arena;

Arena arenaCreate();
void arenaDestroy(Arena arena);
void *arenaAlloc(Arena arena, size_t n, size_t size);
void *arenaRealloc(Arena arena, const void *ptr, size_t oldN, size_t newN, size_t size);