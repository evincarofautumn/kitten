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
  uint64_t data;
  uint64_t type;
} KObject;

typedef struct KActivation {
  uint64_t refs;
  void* function;
  KObject* begin;
  KObject* end;
} KActivation;

typedef struct KPair {
  uint64_t refs;
  KObject first;
  KObject rest;
} KPair;

typedef struct KSole {
  uint64_t refs;
  KObject value;
} KSole;

typedef struct KVector {
  uint64_t refs;
  KObject* begin;
  KObject* end;
  KObject* capacity;
} KVector;

typedef KSole KLeft, KRight, KSome;

typedef union KBoxed {
  uint64_t refs;
  KActivation as_activation;
  KLeft as_left;
  KPair as_pair;
  KRight as_right;
  KSome as_some;
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
KObject k_handle(FILE*);
KObject k_int(int64_t);
KObject k_left(KObject);
KObject k_none(void);
KObject k_pair(KObject, KObject);
KObject k_right(KObject);
KObject k_some(KObject);
KObject k_unit(void);
KObject k_vector(size_t, ...);

KObject k_append_vector(KObject, KObject);
KObject k_make_vector(size_t);
KObject k_new_vector(size_t);

void k_push_data(KObject);
KObject k_pop_data(void);
void k_push_locals(KObject);
KObject k_pop_locals(void);

#define K_DROP_CLOSURE()   (++k_closure)
#define K_DROP_DATA()      ((k_data[0] = (KObject) { .data = K_JUNK, .type = K_JUNK }), ++k_data)
#define K_DROP_LOCALS()    ((k_locals[0] = (KObject) { .data = K_JUNK, .type = K_JUNK }), ++k_locals)
#define K_GET_CLOSURE(i)   (k_closure[0][(i)])
#define K_GET_LOCAL(i)     (k_locals[(i)])
#define K_POP_RETURN()     (*k_return++)
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
    KR call = K_POP_RETURN(); \
    /* TODO Release locals. */ \
    k_locals = call.locals; \
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
    k_push_data(((KVector*)vector.data)->begin[index.data]); \
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
    k_push_data(k_int(((KVector*)vector.data)->end \
      - ((KVector*)vector.data)->begin)); \
  } while (0)

#define K_PAIR() \
  do { \
    KObject b = k_pop_data(); \
    KObject a = k_pop_data(); \
    k_push_data(k_pair(a, b)); \
  } while (0)

#define K_SOME() \
  do { \
    KObject some = k_some(k_pop_data()); \
    k_push_data(some); \
  } while (0)

#define K_BINARY(TYPE, OPERATION) \
  do { \
    KObject b = k_pop_data(); \
    KObject a = k_pop_data(); \
    assert(a.type == b.type); \
    k_push_data(k_##TYPE((k_##TYPE##_t)a.data OPERATION (k_##TYPE##_t)b.data)); \
  } while (0)

#define K_FROM_BOX() \
  do { \
    KObject a = k_pop_data(); \
    k_push_data(*((KObject*)a.data)); \
  } while (0)

#define K_RELATIONAL(TYPE, OPERATION) \
  do { \
    KObject b = k_pop_data(); \
    KObject a = k_pop_data(); \
    assert(a.type == b.type); \
    k_push_data(k_bool((k_##TYPE##_t)a.data OPERATION ((k_##TYPE##_t)b.data))); \
  } while (0)

#define K_UNARY(TYPE, OPERATION) \
  assert(!"TODO unary");

/*
  return [qc|
    {
      if ((*k_data)->refcount == 1) {
        (*k_data)->as_#{ type_ }.value =
          #{ operation }(*k_data)->as_#{ type_ }.value;
      } else {
        KObject* a = *k_data--;
        *++k_data = k_#{ type_ }(#{ operation }a->as_#{ type_ }.value);
      }
    }
  |]
*/

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
    if (x.type != K_INT) { \
      fprintf(stderr, "expected int but got %"PRId64"/%"PRId64"\n", x.data, x.type); \
      assert(x.type == K_INT); \
    } \
    char buffer[20] = {0}; \
    snprintf(buffer, sizeof(buffer), "%"PRId64, (k_int_t)x.data); \
    const size_t length = strnlen(buffer, sizeof(buffer)); \
    KObject string = k_new_vector(length); \
    for (size_t i = 0; i < length; ++i) { \
      ((KVector*)string.data)->begin[i] = (KObject) { \
        .data = buffer[i], \
        .type = K_CHAR \
      }; \
    } \
    k_push_data(string); \
  } while (0)

#endif
