#pragma once
#include <stdio.h>

#include "arena.h"

typedef enum {
    JSON_OBJECT,
    JSON_ARRAY,
    JSON_STRING,
    JSON_NUMBER,
    JSON_BOOLEAN,
    JSON_NULL
} JSONTag;

typedef struct JSONValue JSONValue;
typedef struct JSONKeyValuePair JSONKeyValuePair;

typedef struct {
    size_t count;
    JSONValue *elements;
} JSONArray;

typedef struct {
    size_t count;
    JSONKeyValuePair *pairs;
} JSONObject;

struct JSONValue {
    JSONTag tag;

    union {
        JSONObject object;
        JSONArray array;
        char *string;
        double number;
        bool boolean;
    };
};

typedef struct JSONKeyValuePair {
    char *key;
    JSONValue value;
} JSONKeyValuePair;

JSONValue load(Arena arena, FILE *f);
