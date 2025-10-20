#include <stdlib.h>

#include "common/common.h"

typedef struct {
    int cost, damage, armor;
} Equipment;

constexpr int nWeapons = 5;
constexpr int nArmors = 5;
constexpr int nRings = 6;

Equipment
    weapons[nWeapons] = {
        {8, 4, 0}, {10, 5, 0}, {25, 6, 0},
        {40, 7, 0}, {74, 8, 0},
    }, armors[nArmors] = {
        {13, 0, 1}, {31, 0, 2}, {53, 0, 3},
        {75, 0, 4}, {102, 0, 5},
    }, rings[nRings] = {
        {25, 1, 0}, {50, 2, 0}, {100, 3, 0},
        {20, 0, 1}, {40, 0, 2}, {80, 0, 3},
    };

static Equipment addEquip(Equipment l, Equipment r) {
    return (Equipment){
        .cost = l.cost + r.cost,
        .damage = l.damage + r.damage,
        .armor = l.armor + r.armor,
    };
}

static bool isWin(Equipment equip, int health, Equipment bossEquip, int bossHealth) {
    while (health > 0) {
        bossHealth -= MAX(1, equip.damage - bossEquip.armor);
        if (bossHealth <= 0) { return true; }
        health -= MAX(1, bossEquip.damage - equip.armor);
    }
    return false;
}

static int64_t solvePart0([[maybe_unused]] Arena arena, FILE *f) {
    int bossHp;
    Equipment bossEquip = {0};
    // NOLINTNEXTLINE(cert-err34-c)
    check(fscanf(f, "Hit Points: %d\nDamage: %d\nArmor: %d\n", &bossHp, &bossEquip.damage, &bossEquip.armor) == 3);
    int64_t best = INT_MAX;
    for (int weapon = 0; weapon < nWeapons; weapon++) {
        for (int armor = -1; armor < nArmors; armor++) {
            for (int ring0 = -2; ring0 < nRings; ring0++) {
                for (int ring1 = ring0 + 1; ring1 < nRings; ring1++) {
                    Equipment total = weapons[weapon];
                    if (armor >= 0) { total = addEquip(total, armors[armor]); }
                    if (ring0 >= 0) { total = addEquip(total, rings[ring0]); }
                    if (ring1 >= 0) { total = addEquip(total, rings[ring1]); }
                    if (isWin(total, 100, bossEquip, bossHp)) {
                        best = MIN(best, total.cost);
                    }
                }
            }
        }
    }
    return best;
}

static int64_t solvePart1([[maybe_unused]] Arena arena, FILE *f) {
    int bossHp;
    Equipment bossEquip = {0};
    // NOLINTNEXTLINE(cert-err34-c)
    check(fscanf(f, "Hit Points: %d\nDamage: %d\nArmor: %d\n", &bossHp, &bossEquip.damage, &bossEquip.armor) == 3);
    int64_t best = INT_MIN;
    for (int weapon = 0; weapon < nWeapons; weapon++) {
        for (int armor = -1; armor < nArmors; armor++) {
            for (int ring0 = -2; ring0 < nRings; ring0++) {
                for (int ring1 = ring0 + 1; ring1 < nRings; ring1++) {
                    Equipment total = weapons[weapon];
                    if (armor >= 0) { total = addEquip(total, armors[armor]); }
                    if (ring0 >= 0) { total = addEquip(total, rings[ring0]); }
                    if (ring1 >= 0) { total = addEquip(total, rings[ring1]); }
                    if (!isWin(total, 100, bossEquip, bossHp)) {
                        best = MAX(best, total.cost);
                    }
                }
            }
        }
    }
    return best;
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-21/input-real0.txt", solvePart0, 91, solvePart1, 158);
    return failed;
}

daySolver day21 = {21, dayMain, true};
