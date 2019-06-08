#include <stdlib.h>

#include <ml-types.h>

typedef struct CArray {
    Word8 *ptr;
    Word64 length;
} CArray_t;

CArray_t *CArray_new(Word64 length)
{
    CArray_t *a = malloc(sizeof(CArray_t));
    if (a == NULL) {
        return NULL;
    }
    a->length = length;
    if ((a->ptr = calloc(1, length)) == NULL) {
        free(a);
        return NULL;
    }
    return a;
}

Word8 CArray_sub(CArray_t *array, Word64 i)
{
    if (i >= array->length) {
        return 0;
    }
    return array->ptr[i];
}

Word64 CArray_length(CArray_t *array)
{
    return array->length;
}

void CArray_update(CArray_t *array, Word64 i, Word8 byte)
{
    if (i >= array->length) {
        return;
    }
    array->ptr[i] = byte;
}

void CArray_free(CArray_t *array)
{
    free(array->ptr);
    free(array);
}
