#include "json.h"

#include <ctype.h>
#include <math.h>
#include <stdlib.h>

#include "common.h"
#include "vec.h"

typedef struct {
    FILE *file;
    int peeked;
} ParseState;

static int peek(ParseState *state);

static int get(ParseState *state);

static void eat(ParseState *state, char expected);

static void eatWhitespace(ParseState *state);

static JSONObject parseObject(Arena arena, ParseState *state);

static JSONArray parseArray(Arena arena, ParseState *state);

static char *parseString(Arena arena, ParseState *state);

static double parseNumber(ParseState *state);

static bool parseBoolean(ParseState *state);

static void parseNull(ParseState *state);

static JSONValue parseValue(Arena arena, ParseState *state);

static int peek(ParseState *state) {
    if (state->peeked < 0) {
        state->peeked = fgetc(state->file);
    }
    return state->peeked;
}

static int get(ParseState *state) {
    int result = peek(state);
    state->peeked = -1;
    return result;
}

static void eat(ParseState *state, char expected) {
    check(get(state) == expected);
}

static void eatWhitespace(ParseState *state) {
    while (isspace(peek(state))) {
        get(state);
    }
}

static JSONObject parseObject(Arena arena, ParseState *state) {
    eat(state, '{');
    eatWhitespace(state);
    VEC(JSONKeyValuePair) pairs;
    VEC_INIT(&pairs, arena);
    while (peek(state) != '}') {
        eatWhitespace(state);
        char *key = parseString(arena, state);
        eatWhitespace(state);
        eat(state, ':');
        auto value = parseValue(arena, state);
        VEC_PUSH(pairs, ((JSONKeyValuePair){.key = key, .value = value}));
        switch (peek(state)) {
        case ',':
            eat(state, ',');
        case '}':
            break;
        default:
            fprintf(stderr, "Expected ',' or '}', got '%c'\n", peek(state));
            exit(1);
        }
    }
    eat(state, '}');
    return (JSONObject){.count = VEC_COUNT(pairs), .pairs = VEC_ELEMS(pairs)};
}

static JSONArray parseArray(Arena arena, ParseState *state) {
    eat(state, '[');
    eatWhitespace(state);
    VEC(JSONValue) elements;
    VEC_INIT(&elements, arena);
    while (peek(state) != ']') {
        auto value = parseValue(arena, state);
        VEC_PUSH(elements, value);
        int c;
        switch (c = peek(state)) {
        case ',':
            eat(state, ',');
        case ']':
            break;
        default:
            fprintf(stderr, "Expected ',' or ']', got '%c'\n", c);
            exit(1);
        }
    }
    eat(state, ']');
    return (JSONArray){.count = VEC_COUNT(elements), .elements = VEC_ELEMS(elements)};
}

static char *parseString(Arena arena, ParseState *state) {
    eat(state, '"');
    VecChar result;
    VEC_INIT(&result, arena);
    int c;
    while ((c = get(state)) != '"') {
        if (c != '\\') {
            // TODO: control chars? unicode?
            VEC_PUSH(result, c);
            continue;
        }
        switch (c = get(state)) {
        case '"':
        case '\\':
        case '/':
            VEC_PUSH(result, c);
            break;
        case 'b':
            VEC_PUSH(result, '\b');
            break;
        case 'f':
            VEC_PUSH(result, '\f');
            break;
        case 'n':
            VEC_PUSH(result, '\n');
            break;
        case 'r':
            VEC_PUSH(result, '\r');
            break;
        case 't':
            VEC_PUSH(result, '\t');
            break;
        case 'u':
            // TODO: unicode
        default:
            fprintf(stderr, "Expected an escape sequence, got %c\n", c);
            exit(1);
        }
    }
    return VEC_ELEMS(result);
}

static double parseNumber(ParseState *state) {
    bool negative = false;
    if (peek(state) == '-') {
        eat(state, '-');
        negative = true;
    }
    uint64_t integral = 0;
    int c;
    if ((c = peek(state)) == '0') {
        eat(state, '0');
    } else if ('1' <= c && c <= '9') {
        integral = get(state) - '0';
        while (isdigit(peek(state))) {
            integral = 10 * integral + (get(state) - '0');
        }
    } else if (c != '0') {
        fprintf(stderr, "expected digit, got %c\n", c);
        exit(1);
    }

    // fraction
    uint64_t fraction = 0, inverseFracUnit = 1;
    if (peek(state) == '.') {
        eat(state, '.');
        while (isdigit(peek(state))) {
            fraction = 10 * fraction + (get(state) - '0');
            inverseFracUnit *= 10;
        }
        if (inverseFracUnit == 1) {
            fprintf(stderr, "bad decimal %c\n", c);
            exit(1);
        }
    }

    // exponent
    int64_t exponent = 0;
    c = peek(state);
    if (c == 'e' || c == 'E') {
        get(state);
        bool exponentNegative = false;
        if ((c = peek(state)) == '-') {
            eat(state, '-');
            exponentNegative = true;
        } else if (c == '+') {
            eat(state, '+');
        } else if (!isdigit(c)) {
            fprintf(stderr, "bad decimal %c\n", c);
            exit(1);
        }
        while (isdigit(peek(state))) {
            exponent = 10 * exponent + (get(state) - '0');
        }
        if (exponentNegative) {
            exponent *= -1;
        }
    }

    return
        (negative ? -1 : 1) *
        ((double) integral + (double) fraction / (double) inverseFracUnit) *
        pow(10, (double) exponent);
}

static bool parseBoolean(ParseState *state) {
    if (peek(state) == 't') {
        eat(state, 't');
        eat(state, 'r');
        eat(state, 'u');
        eat(state, 'e');
        return true;
    }
    eat(state, 'f');
    eat(state, 'a');
    eat(state, 'l');
    eat(state, 's');
    eat(state, 'e');
    return false;
}

static void parseNull(ParseState *state) {
    eat(state, 'n');
    eat(state, 'u');
    eat(state, 'l');
    eat(state, 'l');
}

static JSONValue parseValue(Arena arena, ParseState *state) {
    eatWhitespace(state);
    int c;
    switch ((c = peek(state))) {
    case '"': {
        auto string = parseString(arena, state);
        eatWhitespace(state);
        return (JSONValue){JSON_STRING, .string = string};
    }
    case '{': {
        auto obj = parseObject(arena, state);
        eatWhitespace(state);
        return (JSONValue){JSON_OBJECT, .object = obj};
    }
    case '[': {
        auto array = parseArray(arena, state);
        eatWhitespace(state);
        return (JSONValue){JSON_ARRAY, .array = array};
    }
    case 't':
    case 'f': {
        bool boolean = parseBoolean(state);
        eatWhitespace(state);
        return (JSONValue){JSON_BOOLEAN, .boolean = boolean};
    }
    case 'n':
        parseNull(state);
        eatWhitespace(state);
        return (JSONValue){JSON_NULL, {}};
    default:
        if (c == '-' || isdigit(c)) {
            double number = parseNumber(state);
            eatWhitespace(state);
            return (JSONValue){JSON_NUMBER, .number = number};
        }
        fprintf(stderr, "expected json value, found %c\n", c);
        exit(1);
    }
}

JSONValue load(Arena arena, FILE *f) {
    ParseState state = {.file = f, .peeked = -1};
    auto value = parseValue(arena, &state);
    eat(&state, EOF);
    return value;
}
