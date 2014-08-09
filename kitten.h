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

typedef struct KR {
  void* address;
  KObject* locals;
  int closure;
} KR;

extern KR* k_return;
extern KObject** k_closure;
extern KObject* k_data;
extern KObject* k_locals;

// Runtime initialization.
void k_runtime_init(void);
void k_runtime_quit(void);

// Allocation and reference counting.
void* k_alloc(size_t, size_t);
void k_free(void*);
KObject k_retain(KObject);
void k_release(KObject);

// Value construction.
KObject k_new_activation(void *, size_t, ...);
KObject k_new_bool(int);
KObject k_new_char(uint32_t);
KObject k_new_float(double);
KObject k_new_handle(FILE*);
KObject k_new_int(int64_t);
KObject k_new_left(KObject);
KObject k_new_none(void);
KObject k_new_pair(KObject, KObject);
KObject k_new_right(KObject);
KObject k_new_some(KObject);
KObject k_new_unit(void);
KObject k_new_vector(size_t);

// Vector operations.
KObject k_append_vector(KObject, KObject);
KObject k_vector(size_t, ...);
KObject k_vector_get(KObject, k_cell_t);
k_cell_t k_vector_size(KObject);
void k_make_vector(size_t);
void k_vector_set(KObject, k_cell_t, KObject);

// Box operations.
KObject k_box_get(KObject, KType);

// Intrinsics.
void k_add_vector(void);
void k_close(void);
void k_first(void);
void k_from_box(KType);
void k_get(void);
void k_get_line(void);
void k_init(void);
void k_left(void);
void k_length(void);
void k_mod_float(void);
void k_none(void);
void k_pair(void);
void k_print(void);
void k_rest(void);
void k_right(void);
void k_set(void);
void k_show_float(void);
void k_show_int(void);
void k_some(void);
void k_tail(void);

// Stack manipulation.
KObject k_get_closure(k_cell_t);
KObject k_get_local(k_cell_t);
KObject k_pop_data(void);
KObject k_pop_locals(void);
KR k_pop_return(void);
void k_drop_closure(size_t);
void k_drop_data(void);
void k_drop_locals(void);
void k_push_closure(KObject*);
void k_push_data(KObject);
void k_push_locals(KObject);
void k_push_return(KR);

#define K_ACT(LABEL, ...) \
  do { \
    k_push_data(k_new_activation(&&LABEL, __VA_ARGS__)); \
  } while (0)

#define K_CALL(CALL, RETURN) \
  do { \
    k_push_return(((KR){ \
      .address = &&RETURN, \
      .locals = k_locals, \
      .closure = -1, \
    })); \
    goto CALL; \
  } while (0)

#define K_TAIL_CALL(CALL) \
  do { \
    goto CALL; \
  } while (0)

#define K_RETURN() \
  do { \
    const KR call = k_pop_return(); \
    if (call.locals) \
      while (k_locals < call.locals) \
        k_release(*k_locals++); \
    if (call.closure != -1) \
      k_drop_closure(call.closure); \
    goto *call.address; \
  } while (0)

#define K_APPLY(RETURN) \
  do { \
    const KObject object = k_pop_data(); \
    assert(object.type == K_ACTIVATION); \
    const KActivation* const activation = (KActivation*)object.data; \
    const size_t size = activation->end - activation->begin; \
    k_push_closure(k_alloc(size, sizeof(KObject))); \
    for (size_t i = 0; i < size; ++i) \
      k_closure[0][i] = k_retain(activation->begin[i]); \
    k_push_return(((KR){ .address = &&RETURN, .closure = size })); \
    void* const function = activation->function; \
    k_release(object); \
    goto *function; \
  } while (0)

#define CAT_(A, B) A##B
#define CAT(A, B) CAT_(A, B)

#define K_CHOICE() \
  do { \
    const KObject left = k_pop_data(); \
    assert(left.type == K_ACTIVATION); \
    const KObject choice = k_pop_data(); \
    assert(choice.type == K_LEFT || choice.type == K_RIGHT); \
    if (choice.type == K_LEFT) { \
      k_push_data(k_retain(k_box_get(choice, K_LEFT))); \
      k_release(choice); \
      k_push_data(left); \
      K_APPLY(CAT(here, __LINE__)); CAT(here, __LINE__): (void)0; \
    } \
  } while (0)

#define K_CHOICE_ELSE() \
  do { \
    const KObject right = k_pop_data(); \
    assert(right.type == K_ACTIVATION); \
    const KObject left = k_pop_data(); \
    assert(left.type == K_ACTIVATION); \
    const KObject choice = k_pop_data(); \
    assert(choice.type == K_LEFT || choice.type == K_RIGHT); \
    if (choice.type == K_LEFT) { \
      k_push_data(k_retain(k_box_get(choice, K_LEFT))); \
      k_release(choice); \
      k_push_data(left); \
    } else if (choice.type == K_RIGHT) { \
      k_push_data(k_retain(k_box_get(choice, K_RIGHT))); \
      k_release(choice); \
      k_push_data(right); \
    } \
    K_APPLY(CAT(here, __LINE__)); CAT(here, __LINE__): (void)0; \
  } while (0)

#define K_IF() \
  do { \
    const KObject true = k_pop_data(); \
    assert(true.type == K_ACTIVATION); \
    const KObject cond = k_pop_data(); \
    assert(cond.type == K_BOOL); \
    if (cond.data) { \
      k_push_data(true); \
      K_APPLY(CAT(here, __LINE__)); CAT(here, __LINE__): (void)0; \
    } else { \
      k_release(true); \
    } \
  } while (0)

#define K_IF_ELSE() \
  do { \
    const KObject false = k_pop_data(); \
    assert(false.type == K_ACTIVATION); \
    const KObject true = k_pop_data(); \
    assert(true.type == K_ACTIVATION); \
    const KObject cond = k_pop_data(); \
    assert(cond.type == K_BOOL); \
    if (cond.data) { \
      k_push_data(true); \
      k_release(false); \
    } else { \
      k_release(true); \
      k_push_data(false); \
    } \
    K_APPLY(CAT(here, __LINE__)); CAT(here, __LINE__): (void)0; \
  } while (0)

#define K_OPTION() \
  do { \
    const KObject some = k_pop_data(); \
    assert(some.type == K_ACTIVATION); \
    const KObject option = k_pop_data(); \
    assert(option.type == K_SOME || option.type == K_NONE); \
    if (option.type == K_SOME) { \
      k_push_data(k_retain(k_box_get(option, K_SOME))); \
      k_release(option); \
      k_push_data(some); \
      K_APPLY(CAT(here, __LINE__)); CAT(here, __LINE__): (void)0; \
    } else { \
      k_release(option); \
      k_release(some); \
    } \
  } while (0)

#define K_OPTION_ELSE() \
  do { \
    const KObject none = k_pop_data(); \
    assert(none.type == K_ACTIVATION); \
    const KObject some = k_pop_data(); \
    assert(some.type == K_ACTIVATION); \
    const KObject option = k_pop_data(); \
    assert(option.type == K_SOME || option.type == K_NONE); \
    if (option.type == K_SOME) { \
      k_push_data(k_retain(k_box_get(option, K_SOME))); \
      k_release(option); \
      k_push_data(some); \
      k_release(none); \
    } else if (option.type == K_NONE) { \
      k_push_data(none); \
      k_release(some); \
    } \
    K_APPLY(CAT(here, __LINE__)); CAT(here, __LINE__): (void)0; \
  } while (0)

#define K_BINARY(TYPE, OPERATION) \
  do { \
    const KObject b = k_pop_data(); \
    const KObject a = k_pop_data(); \
    assert(a.type == b.type); \
    k_push_data(k_new_##TYPE((k_##TYPE##_t)a.data OPERATION (k_##TYPE##_t)b.data)); \
  } while (0)

#define K_RELATIONAL(TYPE, OPERATION) \
  do { \
    const KObject b = k_pop_data(); \
    const KObject a = k_pop_data(); \
    assert(a.type == b.type); \
    k_push_data(k_new_bool((k_##TYPE##_t)a.data OPERATION ((k_##TYPE##_t)b.data))); \
  } while (0)

#define K_UNARY(TYPE, OPERATION) \
  do { \
    const KObject a = k_pop_data(); \
    k_push_data(k_new_##TYPE(OPERATION (k_##TYPE##_t)a.data)); \
  } while (0)

#endif
