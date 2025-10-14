#include "json.h"

#include <ctype.h>
#include <math.h>
#include <stdlib.h>

#include "common.h"
#include "vec_common.h"

#define VEC_ELEMENT_TYPE JSONValue
#include "vec_impl.c"
#undef VEC_ELEMENT_TYPE

#define VEC_ELEMENT_TYPE JSONKeyValuePair
#include "vec_impl.c"
#undef VEC_ELEMENT_TYPE

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
    auto pairs = vec_JSONKeyValuePair_create(arena);
    while (peek(state) != '}') {
        eatWhitespace(state);
        char *key = parseString(arena, state);
        eatWhitespace(state);
        eat(state, ':');
        auto value = parseValue(arena, state);
        vec_JSONKeyValuePair_push(pairs, (JSONKeyValuePair){.key = key, .value = value});
        int c;
        switch (c = peek(state)) {
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
    return (JSONObject){.count = pairs->count, .pairs = pairs->elements};
}

static JSONArray parseArray(Arena arena, ParseState *state) {
    eat(state, '[');
    eatWhitespace(state);
    auto elements = vec_JSONValue_create(arena);
    while (peek(state) != ']') {
        auto value = parseValue(arena, state);
        vec_JSONValue_push(elements, value);
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
    return (JSONArray){.count = elements->count, .elements = elements->elements};
}

static char *parseString(Arena arena, ParseState *state) {
    eat(state, '"');
    vec_char result = vec_char_create(arena);
    int c;
    while ((c = get(state)) != '"') {
        if (c != '\\') {
            // TODO: control chars? unicode?
            vec_char_push(result, c);
            continue;
        }
        switch (c = get(state)) {
        case '"':
        case '\\':
        case '/':
            vec_char_push(result, c);
            break;
        case 'b':
            vec_char_push(result, '\b');
            break;
        case 'f':
            vec_char_push(result, '\f');
            break;
        case 'n':
            vec_char_push(result, '\n');
            break;
        case 'r':
            vec_char_push(result, '\r');
            break;
        case 't':
            vec_char_push(result, '\t');
            break;
        case 'u':
            // TODO: unicode
        default:
            fprintf(stderr, "Expected an escape sequence, got %c\n", c);
            exit(1);
        }
    }
    return result->elements;
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
        return (JSONValue){JSON_NULL};
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
