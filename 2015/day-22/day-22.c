#include <stdlib.h>
#include <string.h>

#include "common/common.h"
#include "common/heap.h"

typedef struct {
    int manaSpent;
    int mana;
    int hp;
    int bossHp;
    int bossDamage;
    int shieldDuration;
    int poisonDuration;
    int rechargeDuration;
} State;

static int icmp(intptr_t l, intptr_t r) { return l < r ? -1 : l > r ? 1 : 0; }
static int ptrCmp(const void *l, const void *r) { return icmp((intptr_t)l, (intptr_t)r); }

static bool preTurn(State *state, int perTurnDamage) {
    state->hp -= perTurnDamage;
    if (state->hp <= 0) { return false; }
    state->shieldDuration--;
    if (state->poisonDuration-- > 0) { state->bossHp -= 3; }
    if (state->rechargeDuration-- > 0) { state->mana += 101; }
    return true;
}

typedef enum {
    MAGIC_MISSILE, DRAIN, SHIELD, POISON, RECHARGE,
    nAction
} Action;

static void bossTurn(State *state) {
    preTurn(state, 0);
    if (state->bossHp > 0) {
        state->hp -= MAX(1, state->bossDamage - (state->shieldDuration > 0 ? 7 : 0));
    }
}

static bool playerTurn(State *state, Action action, int perTurnDamage) {
    if (!preTurn(state, perTurnDamage)) { return false; }
    switch (action) {
    case MAGIC_MISSILE:
        state->mana -= 53;
        state->manaSpent += 53;
        state->bossHp -= 4;
        return state->mana >= 0;
    case DRAIN:
        state->mana -= 73;
        state->manaSpent += 73;
        state->hp += 2;
        state->bossHp -= 2;
        return state->mana >= 0;
    case SHIELD: {
        state->mana -= 113;
        state->manaSpent += 113;
        auto oldDuration = state->shieldDuration;
        state->shieldDuration = 7;
        return oldDuration <= 0 && state->mana >= 0;
    }
    case POISON: {
        state->mana -= 173;
        state->manaSpent += 173;
        auto oldDuration = state->poisonDuration;;
        state->poisonDuration = 6;
        return oldDuration <= 0 && state->mana >= 0;
    }
    case RECHARGE: {
        state->mana -= 229;
        state->manaSpent += 229;
        auto oldDuration = state->rechargeDuration;;
        state->rechargeDuration = 5;
        return oldDuration <= 0 && state->mana >= 0;
    }
    default:
        exit(1);
    }
}

static State *stateDup(Arena arena, State *s) {
    State *result = arenaAlloc(arena, 1, sizeof(*result));
    memcpy(result, s, sizeof(*result));
    return result;
}

static int64_t solveCommon(Arena arena, FILE *f, int perTurnDamage) {
    State *state = arenaAlloc(arena, 1, sizeof(*state));
    // NOLINTNEXTLINE(cert-err34-c)
    check(fscanf(f, "Hit Points: %d\nDamage: %d\n", &state->bossHp, &state->bossDamage) == 2);
    state->mana = 500;
    state->hp = 50;

    GHeap queue = gheapCreate(arena, ptrCmp);
    gheapInsert(queue, (void *)(intptr_t)state->manaSpent, state);

    while (gheapCount(queue) > 0) {
        auto popped = gheapPopMin(queue);
        state = (State *)popped.value;

        if (state->bossHp <= 0) {
            return state->manaSpent;
        }
        for (Action action = 0; action < nAction; action++) {
            State *nextState = stateDup(arena, state);
            if (!playerTurn(nextState, action, perTurnDamage)) { continue; }
            check(nextState->manaSpent > state->manaSpent);
            bossTurn(nextState);
            if (nextState->hp > 0) {
                gheapInsert(queue, (void *)(intptr_t)nextState->manaSpent, nextState);
            }
        }
    }

    return 0;
}

static int64_t solvePart0(Arena arena, FILE *f) {
    return solveCommon(arena, f, 0);
}

static int64_t solvePart1([[maybe_unused]] Arena arena, FILE *f) {
    return solveCommon(arena, f, 1);
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-22/input-real0.txt", solvePart0, 953, solvePart1, 1289);
    return failed;
}

daySolver day22 = {22, dayMain, true};
