#ifndef KITTEN_H
#define KITTEN_H

#include <assert.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int k_bool_t;
typedef uint32_t k_char_t;
typedef double k_float_t;
typedef FILE* k_handle_t;
typedef int64_t k_int_t;

typedef uint64_t k_cell_t;

typedef enum KClosedName {
  K_CLOSED,
  K_RECLOSED
} KClosedName;

typedef enum KType {
  K_JUNK = -1,
  K_UNBOXED = 0x00,
  K_BOOL = K_UNBOXED + 1,
  K_CHAR,
  K_FLOAT,
  K_INT,
  K_NONE,
  K_UNIT,
  K_BOXED = 0x10,
  K_ACTIVATION = K_BOXED + 1,
  K_HANDLE,
  K_LEFT,
  K_PAIR,
  K_RIGHT,
  K_SOME,
  K_VECTOR,
  K_MAX_TYPE
} KType;

typedef struct KObject {
  k_cell_t data;
  k_cell_t type;
} KObject;

typedef struct KActivation {
  k_cell_t refs;
  void* function;
  KObject* begin;
  KObject* end;
} KActivation;

typedef struct KPair {
  k_cell_t refs;
  KObject first;
  KObject rest;
} KPair;

typedef struct KBox {
  k_cell_t refs;
  KObject value;
} KBox;

typedef struct KVector {
  k_cell_t refs;
  KObject* begin;
  KObject* end;
  KObject* capacity;
} KVector;

typedef union KBoxed {
  k_cell_t refs;
  KActivation as_activation;
  KBox as_box;
  KPair as_pair;
  KVector as_vector;
} KBoxed;

typedef struct KR {
  void* address;
  KObject* locals;
  int closure;
} KR;

extern KR* k_return;
extern KObject** k_closure;
extern KObject* k_data;
extern KObject* k_locals;

void k_init(void);

KObject k_retain(KObject);
KObject k_release(KObject);

KObject k_activation(void *, size_t, ...);
KObject k_bool(int);
KObject k_char(uint32_t);
KObject k_float(double);
KObject k_from_box(KObject, KType);
KObject k_handle(FILE*);
KObject k_int(int64_t);
KObject k_left(KObject);
KObject k_none(void);
KObject k_pair(KObject, KObject);
KObject k_right(KObject);
KObject k_some(KObject);
KObject k_unit(void);
KObject k_vector(size_t, ...);
KObject k_vector_get(KObject, k_cell_t);
void k_vector_set(KObject, k_cell_t, KObject);
k_cell_t k_vector_size(KObject);

KObject k_append_vector(KObject, KObject);
KObject k_make_vector(size_t);
KObject k_new_vector(size_t);

void k_push_data(KObject);
KObject k_pop_data(void);
void k_push_locals(KObject);
KObject k_pop_locals(void);
KR k_pop_return(void);

#define K_DROP_CLOSURE()   (++k_closure)
#define K_DROP_DATA()      ((k_data[0] = (KObject) { .data = K_JUNK, .type = K_JUNK }), ++k_data)
#define K_DROP_LOCALS()    ((k_locals[0] = (KObject) { .data = K_JUNK, .type = K_JUNK }), ++k_locals)
#define K_GET_CLOSURE(i)   (k_closure[0][(i)])
#define K_GET_LOCAL(i)     (k_locals[(i)])
#define K_PUSH_CLOSURE(x)  (*--k_closure = (x))
#define K_PUSH_RETURN(x)   (*--k_return = (x))

#define K_ACT(LABEL, ...) \
  do { \
    k_push_data(k_activation(&&LABEL, __VA_ARGS__)); \
  } while (0)

#define K_CALL(CALL, RETURN) \
  do { \
    K_PUSH_RETURN(((KR){ \
      .address = &&RETURN, \
      .locals = k_locals, \
      .closure = 0, \
    })); \
    goto CALL; \
  } while (0)

#define K_TAIL_CALL(CALL) \
  do { \
    goto CALL; \
  } while (0)

#define K_RETURN() \
  do { \
    KR call = k_pop_return(); \
    /* TODO Release locals. */ \
    if (call.locals) { \
      k_locals = call.locals; \
    } \
    if (call.closure) { \
      /* TODO Release closure. */ \
      K_DROP_CLOSURE(); \
    } \
    goto *call.address; \
  } while (0)

#define K_MAKE_VECTOR(SIZE) \
  do { \
    const KObject vector = k_make_vector(SIZE); \
    k_push_data(vector); \
  } while (0)

#define K_ADD_VECTOR() \
  do { \
    KObject b = k_pop_data(); \
    KObject a = k_pop_data(); \
    assert(a.type == K_VECTOR); \
    assert(b.type == K_VECTOR); \
    k_push_data(k_append_vector(a, b)); \
  } while (0)

#define K_APPLY(RETURN) \
  do { \
    KObject object = k_pop_data(); \
    assert(object.type == K_ACTIVATION); \
    const KActivation* const activation = (KActivation*)object.data; \
    const size_t size = activation->end - activation->begin; \
    K_PUSH_CLOSURE(calloc(size, sizeof(KObject))); \
    for (size_t i = 0; i < size; ++i) { \
      k_closure[0][i] = activation->begin[i]; \
    } \
    K_PUSH_RETURN(((KR){ .address = &&RETURN, .closure = 1 })); \
    void* const function = activation->function; \
    goto *function; \
  } while (0)

#define K_CLOSE() \
  do { \
    KObject handle = k_pop_data(); \
    assert(handle.type == K_HANDLE); \
    fclose((FILE*)handle.data); \
  } while (0)

#define K_FIRST() \
  do { \
    KObject pair = k_pop_data(); \
    assert(pair.type == K_PAIR); \
    k_push_data(((KPair*)pair.data)->first); \
  } while (0)

#define K_GET() \
  do { \
    KObject index = k_pop_data(); \
    assert(index.type == K_INT); \
    KObject vector = k_pop_data(); \
    assert(vector.type == K_VECTOR); \
    const k_int_t i = (k_int_t)index.data; \
    const k_cell_t size = k_vector_size(vector); \
    k_push_data(i < 0 || i >= size \
      ? k_none() : k_some(k_vector_get(vector, i)));\
  } while (0)

#define CAT_(A, B) A##B
#define CAT(A, B) CAT_(A, B)

#define K_IF() \
  do { \
    KObject true = k_pop_data(); \
    assert(true.type == K_ACTIVATION); \
    KObject cond = k_pop_data(); \
    assert(cond.type == K_BOOL); \
    if (cond.data) { \
      k_push_data(true); \
      K_APPLY(CAT(here, __LINE__)); CAT(here, __LINE__): (void)0; \
    } \
  } while (0)

#define K_IF_ELSE() \
  do { \
    KObject false = k_pop_data(); \
    assert(false.type == K_ACTIVATION); \
    KObject true = k_pop_data(); \
    assert(true.type == K_ACTIVATION); \
    KObject cond = k_pop_data(); \
    assert(cond.type == K_BOOL); \
    if (cond.data) { \
      k_push_data(true); \
    } else { \
      k_push_data(false); \
    } \
    K_APPLY(CAT(here, __LINE__)); CAT(here, __LINE__): (void)0; \
  } while (0)

/* TODO Common up with K_TAIL(). */
#define K_INIT() \
  do { \
    KObject vector = k_pop_data(); \
    assert(vector.type == K_VECTOR); \
    const k_cell_t size = k_vector_size(vector); \
    const KObject result = k_new_vector(size == 0 ? 0 : size - 1); \
    for (k_cell_t i = 0; i + 1 < size; ++i) { \
      k_vector_set(result, i, k_vector_get(vector, i)); \
    } \
    k_push_data(result); \
  } while (0)

#define K_LEFT() \
  do { \
    KObject left = k_left(k_pop_data()); \
    k_push_data(left); \
  } while (0)

#define K_RIGHT() \
  do { \
    KObject right = k_right(k_pop_data()); \
    k_push_data(right); \
  } while (0)

#define K_REST() \
  do { \
    KObject pair = k_pop_data(); \
    assert(pair.type == K_PAIR); \
    k_push_data(((KPair*)pair.data)->rest); \
  } while (0)

#define K_LENGTH() \
  do { \
    KObject vector = k_pop_data(); \
    assert(vector.type == K_VECTOR); \
    k_push_data(k_int(k_vector_size(vector))); \
  } while (0)

#define K_OPTION() \
  assert(!"TODO __option");

#define K_OPTION_ELSE() \
  do { \
    KObject none = k_pop_data(); \
    assert(none.type == K_ACTIVATION); \
    KObject some = k_pop_data(); \
    assert(some.type == K_ACTIVATION); \
    KObject option = k_pop_data(); \
    assert(option.type == K_SOME || option.type == K_NONE); \
    if (option.type == K_SOME) { \
      k_push_data(k_from_box(option, K_SOME)); \
      k_push_data(some); \
    } else if (option.type == K_NONE) { \
      k_push_data(none); \
    } \
    K_APPLY(CAT(here, __LINE__)); CAT(here, __LINE__): (void)0; \
  } while (0)

#define K_PAIR() \
  do { \
    KObject b = k_pop_data(); \
    KObject a = k_pop_data(); \
    k_push_data(k_pair(a, b)); \
  } while (0)

/* TODO Copy on write. */
#define K_SET() \
  do { \
    KObject value = k_pop_data(); \
    KObject index = k_pop_data(); \
    assert(index.type == K_INT); \
    KObject vector = k_pop_data(); \
    assert(vector.type == K_VECTOR); \
    const k_cell_t size = k_vector_size(vector); \
    const KObject result = k_new_vector(size); \
    for (k_cell_t i = 0; i < size; ++i) { \
      if (i == index.data) { \
        k_vector_set(result, i, value); \
        continue; \
      } \
      k_vector_set(result, i, k_vector_get(vector, i)); \
    } \
    k_push_data(result); \
  } while (0)

#define K_SOME() \
  do { \
    KObject some = k_some(k_pop_data()); \
    k_push_data(some); \
  } while (0)

#define K_TAIL() \
  do { \
    KObject vector = k_pop_data(); \
    assert(vector.type == K_VECTOR); \
    const k_cell_t size = k_vector_size(vector); \
    const KObject result = k_new_vector(size == 0 ? 0 : size - 1); \
    for (k_cell_t i = 0; i + 1 < size; ++i) { \
      k_vector_set(result, i, k_vector_get(vector, i + 1)); \
    } \
    k_push_data(result); \
  } while (0)

#define K_BINARY(TYPE, OPERATION) \
  do { \
    KObject b = k_pop_data(); \
    KObject a = k_pop_data(); \
    assert(a.type == b.type); \
    k_push_data(k_##TYPE((k_##TYPE##_t)a.data OPERATION (k_##TYPE##_t)b.data)); \
  } while (0)

#define K_FROM_BOX(TYPE) \
  do { \
    KObject a = k_pop_data(); \
    k_push_data(k_from_box(a, TYPE)); \
  } while (0)

#define K_RELATIONAL(TYPE, OPERATION) \
  do { \
    KObject b = k_pop_data(); \
    KObject a = k_pop_data(); \
    assert(a.type == b.type); \
    k_push_data(k_bool((k_##TYPE##_t)a.data OPERATION ((k_##TYPE##_t)b.data))); \
  } while (0)

#define K_UNARY(TYPE, OPERATION) \
  do { \
    KObject a = k_pop_data(); \
    k_push_data(k_##TYPE(OPERATION (k_##TYPE##_t)a.data)); \
  } while (0)

#define K_PRINT() \
  do { \
    KObject handle = k_pop_data(); \
    assert(handle.type == K_HANDLE); \
    KObject string = k_pop_data(); \
    assert(string.type == K_VECTOR); \
    for (KObject* p = ((KVector*)string.data)->begin; \
         p != ((KVector*)string.data)->end; ++p) { \
      fputc(p->data, (FILE*)handle.data); \
    } \
  } while (0)

#define K_SHOW_INT() \
  do { \
    KObject x = k_pop_data(); \
    assert(x.type == K_INT);  \
    char buffer[20] = {0}; \
    int length = 0; \
    snprintf(buffer, sizeof(buffer), \
      "%"PRId64"%n", (k_int_t)x.data, &length);  \
    KObject string = k_new_vector(length); \
    for (size_t i = 0; i < length; ++i) { \
      k_vector_set(string, i, k_char(buffer[i])); \
    } \
    k_push_data(string); \
  } while (0)

#endif
