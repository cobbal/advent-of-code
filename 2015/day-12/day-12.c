#include <stdlib.h>
#include <string.h>

#include "common/common.h"
#include "common/json.h"

static int64_t theSumOfAllCyan(JSONValue thisValue, const char *forbidden) {
    switch (thisValue.tag) {
    case JSON_OBJECT: {
        auto obj = thisValue.object;
        int64_t sum = 0;
        for (size_t i = 0; i < obj.count; i++) {
            auto value = obj.pairs[i].value;
            if (forbidden && value.tag == JSON_STRING && strcmp(value.string, forbidden) == 0) {
                return 0;
            }
            sum += theSumOfAllCyan(value, forbidden);
        }
        return sum;
    }
    case JSON_ARRAY:
        auto arr = thisValue.array;
        int64_t sum = 0;
        for (size_t i = 0; i < arr.count; i++) {
            sum += theSumOfAllCyan(arr.elements[i], forbidden);
        }
        return sum;
    case JSON_NUMBER:
        return (int64_t) thisValue.number;
    case JSON_STRING:
    case JSON_BOOLEAN:
    case JSON_NULL:
        return 0;
    default:
        exit(1);
    }
}

static int64_t solvePart0(Arena arena, FILE *f) {
    auto json = load(arena, f);
    return theSumOfAllCyan(json, nullptr);
}

static int64_t solvePart1(Arena arena, FILE *f) {
    auto json = load(arena, f);
    return theSumOfAllCyan(json, "red");
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-12/input-ex0.txt", solvePart0, 3, solvePart1, 3);
    failed += checkInputInt("day-12/input-real0.txt", solvePart0, 156366, solvePart1, 96852);
    return failed;
}

daySolver day12 = {12, dayMain, true};
