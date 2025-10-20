#include <stdlib.h>

#include "common/common.h"

typedef struct {
    char *name;
    int64_t capacity, durability, flavor, texture, calories;
} Ingredient;
typedef VEC(Ingredient) VecIngredient;

VecIngredient readIngredients(Arena arena, FILE *f) {
    VecString words;
    VecIngredient ingredients;
    VEC_INIT(&ingredients, arena);
    while (readLineWords(arena, f, &words) && VEC_COUNT(words) == 11) {
        auto w = VEC_ELEMS(words);
        // NOLINTNEXTLINE(cert-err34-c)
        Ingredient ingr = {w[0], atoi(w[2]), atoi(w[4]), atoi(w[6]), atoi(w[8]), atoi(w[10])};
        VEC_PUSH(ingredients, ingr);
    }
    return ingredients;
}

static int64_t score(VecIngredient ingredients, const int *quantities, int64_t exactCalories) {
    Ingredient total = {};
    for (size_t i = 0; i < VEC_COUNT(ingredients); i++) {
        Ingredient ingr = VEC_ELEMS(ingredients)[i];
        total.capacity += quantities[i] * ingr.capacity;
        total.durability += quantities[i] * ingr.durability;
        total.flavor += quantities[i] * ingr.flavor;
        total.texture += quantities[i] * ingr.texture;
        total.calories += quantities[i] * ingr.calories;
    }
    if (exactCalories != 0 && total.calories != exactCalories) { return 0; }
    return
        MAX(0, total.capacity) *
        MAX(0, total.durability) *
        MAX(0, total.flavor) *
        MAX(0, total.texture);
}

static bool incrementPartition(size_t buckets, int *allocations) {
    int *spares = &allocations[buckets - 1];
    for (int i = (int)buckets - 2; 0 <= i; i--) {
        if (*spares > 0) {
            allocations[i]++;
            (*spares)--;
            return true;
        }
        if (i > 0 && allocations[i] > 0) {
            allocations[i - 1]++;
            *spares = allocations[i] - 1;
            allocations[i] = 0;
            return true;
        }
    }
    return false;
}

static int64_t solvePart0(Arena arena, FILE *f) {
    auto ingredients = readIngredients(arena, f);
    int *quantities = arenaAlloc(arena, VEC_COUNT(ingredients), sizeof(*quantities));
    quantities[VEC_COUNT(ingredients) - 1] = 100;
    int64_t bestScore = 0;
    do {
        bestScore = MAX(bestScore, score(ingredients, quantities, 0));
    } while(incrementPartition(VEC_COUNT(ingredients), quantities));
    return bestScore;
}

static int64_t solvePart1([[maybe_unused]] Arena arena, FILE *f) {
    auto ingredients = readIngredients(arena, f);
    int *quantities = arenaAlloc(arena, VEC_COUNT(ingredients), sizeof(*quantities));
    quantities[VEC_COUNT(ingredients) - 1] = 100;
    int64_t bestScore = 0;
    do {
        bestScore = MAX(bestScore, score(ingredients, quantities, 500));
    } while(incrementPartition(VEC_COUNT(ingredients), quantities));
    return bestScore;
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-15/input-ex0.txt", solvePart0, 62842880, solvePart1, 57600000);
    failed += checkInputInt("day-15/input-real0.txt", solvePart0, 21367368, solvePart1, 1766400);
    return failed;
}

daySolver day15 = {15, dayMain, true};
