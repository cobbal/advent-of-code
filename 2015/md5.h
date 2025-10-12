#pragma once
#include <stdint.h>

typedef struct {
    uint8_t digest[16];
} MD5Digest;

MD5Digest md5(char *msg, size_t len);
