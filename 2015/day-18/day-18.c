#include <string.h>

#include "common/common.h"
#include "common/vec_common.h"

#define IX(n, y, x) (((y) + 1) * ((n) + 2) + (x) + 1)

typedef struct {
    size_t n;
    uint8_t *grid;
    uint8_t *scratch;
} Grid;

[[maybe_unused]]
static void printGrid(Grid grid) {
    fprintf(stderr, "\n");
    for (size_t y = 0; y < grid.n; y++) {
        for (size_t x = 0; x < grid.n; x++) {
            fputc(grid.grid[IX(grid.n, y, x)] ? '#' : '.', stderr);
        }
        fprintf(stderr, "\n");
    }
}

static void step(Grid *grid) {
    for (ssize_t y = 0; y < (ssize_t) grid->n; y++) {
        for (ssize_t x = 0; x < (ssize_t) grid->n; x++) {
            uint8_t neighborsOn = 0;
            for (int dy = -1; dy <= 1; dy++) {
                for (int dx = -1; dx <= 1; dx++) {
                    neighborsOn += grid->grid[IX(grid->n, y + dy, x + dx)];
                }
            }
            if (grid->grid[IX(grid->n, y, x)]) {
                grid->scratch[IX(grid->n, y, x)] = neighborsOn == 3 || neighborsOn == 4;
            } else {
                grid->scratch[IX(grid->n, y, x)] = neighborsOn == 3;
            }
        }
    }
    uint8_t *tmp = grid->grid;
    grid->grid = grid->scratch;
    grid->scratch = tmp;
}

static Grid readGrid(Arena arena, FILE *f) {
    vec_string lines = vec_string_create(arena);
    char *line;
    while ((line = readLine(arena, f))) {
        vec_string_push(lines, line);
    }
    Grid grid = {};
    grid.n = lines->count;
    for (size_t i = 0; i < grid.n; i++) {
        check(strlen(lines->elements[i]) == grid.n);
    }
    grid.grid = arenaAlloc(arena, (grid.n + 2) * (grid.n + 2), 1);
    grid.scratch = arenaAlloc(arena, (grid.n + 2) * (grid.n + 2), 1);
    for (size_t y = 0; y < grid.n; y++) {
        for (size_t x = 0; x < grid.n; x++) {
            grid.grid[IX(grid.n, y, x)] = lines->elements[y][x] == '#';
        }
    }
    return grid;
}

static int64_t countGrid(Grid grid) {
    int64_t count = 0;
    for (size_t y = 0; y < grid.n; y++) {
        for (size_t x = 0; x < grid.n; x++) {
            count += grid.grid[IX(grid.n, y, x)];
        }
    }
    return count;
}

static int64_t solvePart0N(Arena arena, FILE *f, int steps) {
    Grid grid = readGrid(arena, f);
    for (int i = 0; i < steps; i++) {
        step(&grid);
    }
    // printGrid(grid);
    return countGrid(grid);
}

static int64_t solvePart0Ex(Arena arena, FILE *f) {
    return solvePart0N(arena, f, 4);
}

static int64_t solvePart0Real(Arena arena, FILE *f) {
    return solvePart0N(arena, f, 100);
}

static int64_t solvePart1N(Arena arena, FILE *f, int steps) {
    Grid grid = readGrid(arena, f);
    auto z = grid.n - 1;
    grid.grid[IX(grid.n, 0, 0)] = 1;
    grid.grid[IX(grid.n, z, 0)] = 1;
    grid.grid[IX(grid.n, 0, z)] = 1;
    grid.grid[IX(grid.n, z, z)] = 1;
    for (int i = 0; i < steps; i++) {
        step(&grid);
        grid.grid[IX(grid.n, 0, 0)] = 1;
        grid.grid[IX(grid.n, z, 0)] = 1;
        grid.grid[IX(grid.n, 0, z)] = 1;
        grid.grid[IX(grid.n, z, z)] = 1;
        // printGrid(grid);
    }
    return countGrid(grid);
}

static int64_t solvePart1Ex(Arena arena, FILE *f) {
    return solvePart1N(arena, f, 5);
}

static int64_t solvePart1Real(Arena arena, FILE *f) {
    return solvePart1N(arena, f, 100);
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-18/input-ex0.txt", solvePart0Ex, 4, solvePart1Ex, 17);
    failed += checkInputInt("day-18/input-real0.txt", solvePart0Real, 1061, solvePart1Real, 1006);
    return failed;
}

daySolver day18 = {18, dayMain, true};
