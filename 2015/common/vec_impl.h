#if ! defined(VEC_ELEMENT_TYPE)
#error "must define VEC_ELEMENT_TYPE before including this file"
#endif

#define __VEC_IMPL_PASTE(a, b) __VEC_IMPL_PASTE_IMPL(a, b)
#define __VEC_IMPL_PASTE_IMPL(a, b) a##b
#if !defined(VEC_NAME)
#define __VEC_IMPL_DEFAULT_NAME
#define VEC_NAME __VEC_IMPL_PASTE(vec_, VEC_ELEMENT_TYPE)
#endif

typedef struct __VEC_IMPL_PASTE(VEC_NAME, _impl) {
    Arena arena;
    size_t count;
    size_t capacity;
    VEC_ELEMENT_TYPE *elements;
} *VEC_NAME;

VEC_NAME __VEC_IMPL_PASTE(VEC_NAME, _create)(Arena arena);
VEC_NAME __VEC_IMPL_PASTE(VEC_NAME, _createAndFill)(Arena arena, size_t count, VEC_ELEMENT_TYPE fill);
void __VEC_IMPL_PASTE(VEC_NAME, _setCapacity)(VEC_NAME vec, size_t newCapacity);
void __VEC_IMPL_PASTE(VEC_NAME, _push)(VEC_NAME vec, VEC_ELEMENT_TYPE elem);

#undef __VEC_IMPL_PASTE
#undef __VEC_IMPL_PASTE_IMPL
#ifdef __VEC_IMPL_DEFAULT_NAME
#undef VEC_NAME
#undef __VEC_IMPL_DEFAULT_NAME
#endif
