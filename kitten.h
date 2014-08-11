#ifndef KITTEN_H
#define KITTEN_H

#include <assert.h>
#include <float.h>
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
  K_HANDLE,
  K_INT,
  K_NONE,
  K_UNIT,
  K_BOXED = 0x10,
  K_ACTIVATION = K_BOXED + 1,
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

typedef struct KCall {
  void* address;
  KObject* locals;
  int closure;
} KCall;

extern KCall* k_call;
extern KObject** k_closure;
extern KObject* k_data;
extern KObject* k_locals;

// Runtime initialization.
void k_runtime_init(void);
void k_runtime_quit(void);

// Memory and reference counting.
void* k_mem_alloc(size_t, size_t);
void k_mem_free(void*);
void* k_mem_realloc(void*, size_t, size_t);
KObject k_object_retain(KObject);
void k_object_release(KObject);
int k_object_unique(KObject);

// Value creation.
KObject k_activation_new(void*, size_t, ...);
KObject k_bool_new(int);
KObject k_char_new(uint32_t);
KObject k_float_new(double);
KObject k_handle_new(FILE*);
KObject k_int_new(int64_t);
KObject k_left_new(KObject);
KObject k_none_new(void);
KObject k_pair_new(KObject, KObject);
KObject k_right_new(KObject);
KObject k_some_new(KObject);
KObject k_unit_new(void);
KObject k_vector_new(size_t);

// Box operations.
KObject k_box_get(KObject, KType);

// Pair operations.
KObject k_pair_first(KObject);
KObject k_pair_rest(KObject);

// Vector operations.
KObject k_vector(size_t, ...);
KObject k_vector_append(KObject, KObject);
KObject k_vector_get(KObject, k_cell_t);
k_cell_t k_vector_size(KObject);
void k_vector_set(KObject, k_cell_t, KObject);

// Intrinsics.
void k_in_add_vector(void);
void k_in_close(void);
void k_in_first(void);
void k_in_from_box(KType);
void k_in_get(void);
void k_in_get_line(void);
void k_in_init(void);
void k_in_left(void);
void k_in_length(void);
void k_in_make_vector(size_t);
void k_in_mod_float(void);
void k_in_pair(void);
void k_in_print(void);
void k_in_rest(void);
void k_in_right(void);
void k_in_set(void);
void k_in_show_float(void);
void k_in_show_int(void);
void k_in_some(void);
void k_in_tail(void);

// Stack manipulation.
KCall k_call_pop(void);
KObject k_closure_get(k_cell_t);
KObject k_data_pop(void);
KObject k_locals_get(k_cell_t);
void k_call_push(KCall);
void k_closure_drop(size_t);
void k_closure_push(KObject*);
void k_data_drop(void);
void k_data_push(KObject);
void k_locals_drop(void);
void k_locals_push(KObject);

#define K_IN_ACT(LABEL, ...) \
  do { \
    k_data_push(k_activation_new(&&LABEL, __VA_ARGS__)); \
  } while (0)

#define K_IN_CALL(CALL, RETURN) \
  do { \
    k_call_push(((KCall){ \
      .address = &&RETURN, \
      .locals = k_locals, \
      .closure = -1, \
    })); \
    goto CALL; \
  } while (0)

#define K_IN_TAIL_CALL(CALL) \
  do { \
    goto CALL; \
  } while (0)

#define K_IN_RETURN() \
  do { \
    const KCall call = k_call_pop(); \
    if (call.locals) \
      while (k_locals < call.locals) \
        k_object_release(*k_locals++); \
    if (call.closure != -1) \
      k_closure_drop(call.closure); \
    goto *call.address; \
  } while (0)

#define K_IN_APPLY(RETURN) \
  do { \
    const KObject object = k_data_pop(); \
    assert(object.type == K_ACTIVATION); \
    const KActivation* const activation = (KActivation*)object.data; \
    const size_t size = activation->end - activation->begin; \
    k_closure_push(k_mem_alloc(size, sizeof(KObject))); \
    for (size_t i = 0; i < size; ++i) \
      k_closure[0][i] = k_object_retain(activation->begin[i]); \
    k_call_push(((KCall){ \
      .address = &&RETURN, \
      .locals = k_locals, \
      .closure = size \
    })); \
    void* const function = activation->function; \
    k_object_release(object); \
    goto *function; \
  } while (0)

#define CAT_(A, B) A##B
#define CAT(A, B) CAT_(A, B)

#define K_IN_CHOICE() \
  do { \
    const KObject left = k_data_pop(); \
    assert(left.type == K_ACTIVATION); \
    const KObject choice = k_data_pop(); \
    assert(choice.type == K_LEFT || choice.type == K_RIGHT); \
    if (choice.type == K_LEFT) { \
      k_data_push(k_object_retain(k_box_get(choice, K_LEFT))); \
      k_object_release(choice); \
      k_data_push(left); \
      K_IN_APPLY(CAT(here, __LINE__)); CAT(here, __LINE__): (void)0; \
    } \
  } while (0)

#define K_IN_CHOICE_ELSE() \
  do { \
    const KObject right = k_data_pop(); \
    assert(right.type == K_ACTIVATION); \
    const KObject left = k_data_pop(); \
    assert(left.type == K_ACTIVATION); \
    const KObject choice = k_data_pop(); \
    assert(choice.type == K_LEFT || choice.type == K_RIGHT); \
    if (choice.type == K_LEFT) { \
      k_data_push(k_object_retain(k_box_get(choice, K_LEFT))); \
      k_object_release(choice); \
      k_data_push(left); \
    } else if (choice.type == K_RIGHT) { \
      k_data_push(k_object_retain(k_box_get(choice, K_RIGHT))); \
      k_object_release(choice); \
      k_data_push(right); \
    } \
    K_IN_APPLY(CAT(here, __LINE__)); CAT(here, __LINE__): (void)0; \
  } while (0)

#define K_IN_IF() \
  do { \
    const KObject true = k_data_pop(); \
    assert(true.type == K_ACTIVATION); \
    const KObject cond = k_data_pop(); \
    assert(cond.type == K_BOOL); \
    if (cond.data) { \
      k_data_push(true); \
      K_IN_APPLY(CAT(here, __LINE__)); CAT(here, __LINE__): (void)0; \
    } else { \
      k_object_release(true); \
    } \
  } while (0)

#define K_IN_IF_ELSE() \
  do { \
    const KObject false = k_data_pop(); \
    assert(false.type == K_ACTIVATION); \
    const KObject true = k_data_pop(); \
    assert(true.type == K_ACTIVATION); \
    const KObject cond = k_data_pop(); \
    assert(cond.type == K_BOOL); \
    if (cond.data) { \
      k_data_push(true); \
      k_object_release(false); \
    } else { \
      k_object_release(true); \
      k_data_push(false); \
    } \
    K_IN_APPLY(CAT(here, __LINE__)); CAT(here, __LINE__): (void)0; \
  } while (0)

#define K_IN_OPTION() \
  do { \
    const KObject some = k_data_pop(); \
    assert(some.type == K_ACTIVATION); \
    const KObject option = k_data_pop(); \
    assert(option.type == K_SOME || option.type == K_NONE); \
    if (option.type == K_SOME) { \
      k_data_push(k_object_retain(k_box_get(option, K_SOME))); \
      k_object_release(option); \
      k_data_push(some); \
      K_IN_APPLY(CAT(here, __LINE__)); CAT(here, __LINE__): (void)0; \
    } else { \
      k_object_release(option); \
      k_object_release(some); \
    } \
  } while (0)

#define K_IN_OPTION_ELSE() \
  do { \
    const KObject none = k_data_pop(); \
    assert(none.type == K_ACTIVATION); \
    const KObject some = k_data_pop(); \
    assert(some.type == K_ACTIVATION); \
    const KObject option = k_data_pop(); \
    assert(option.type == K_SOME || option.type == K_NONE); \
    if (option.type == K_SOME) { \
      k_data_push(k_object_retain(k_box_get(option, K_SOME))); \
      k_object_release(option); \
      k_data_push(some); \
      k_object_release(none); \
    } else if (option.type == K_NONE) { \
      k_data_push(none); \
      k_object_release(some); \
    } \
    K_IN_APPLY(CAT(here, __LINE__)); CAT(here, __LINE__): (void)0; \
  } while (0)

#define K_IN_BINARY(TYPE, OPERATION) \
  do { \
    const KObject b = k_data_pop(); \
    KObject* const a = &k_data[0]; \
    assert(a->type == b.type); \
    k_##TYPE##_t result \
      = (*(k_##TYPE##_t*)&a->data) OPERATION (*(k_##TYPE##_t*)&b.data); \
    a->data = *(k_cell_t*)&result; \
  } while (0)

#define K_IN_RELATIONAL(TYPE, OPERATION) \
  do { \
    const KObject b = k_data_pop(); \
    KObject* const a = &k_data[0]; \
    assert(a->type == b.type); \
    a->data = (*(k_##TYPE##_t*)&a->data) OPERATION (*(k_##TYPE##_t*)&b.data); \
  } while (0)

#define K_IN_UNARY(TYPE, OPERATION) \
  do { \
    KObject* const a = &k_data[0]; \
    const k_##TYPE##_t result = OPERATION (*(k_##TYPE##_t*)&a->data); \
    a->data = *(k_cell_t*)&result; \
  } while (0)

#endif
