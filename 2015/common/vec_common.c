#include <stdint.h>
#include "arena.h"

#define VEC_ELEMENT_TYPE int64_t
#define VEC_NAME vec_int64
#include "vec_impl.c"
#undef VEC_ELEMENT_TYPE
#undef VEC_NAME

#define VEC_ELEMENT_TYPE char
#include "vec_impl.c"
#undef VEC_ELEMENT_TYPE

#define VEC_ELEMENT_TYPE char*
#define VEC_NAME vec_string
#include "vec_impl.c"
#undef VEC_ELEMENT_TYPE
#undef VEC_NAME


