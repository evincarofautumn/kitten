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

struct KActivation;
struct KBox;
struct KPair;
struct KVector;

typedef union KData {
  k_bool_t as_bool;
  k_char_t as_char;
  k_int_t as_int;
  k_float_t as_float;
  k_handle_t as_handle;
  k_cell_t* as_refs;
  struct KActivation* as_activation;
  struct KBox* as_box;
  struct KPair* as_pair;
  struct KVector* as_vector;
} KData;

typedef struct KObject {
  KType type;
  KData data;
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

static inline KObject k_object_retain(const KObject object) {
  if (object.type > K_BOXED) ++*object.data.as_refs;
  return object;
}

void k_object_release(KObject);
int k_object_unique(KObject);

// Value creation.
KObject k_activation_new(void*, size_t, ...);
KObject k_left_new(KObject);
KObject k_pair_new(KObject, KObject);
KObject k_right_new(KObject);
KObject k_some_new(KObject);
KObject k_vector_new(size_t);

// Vector operations.
KObject k_vector(size_t, ...);
KObject k_vector_append(KObject, KObject);
KObject k_vector_get(KObject, k_cell_t);
k_cell_t k_vector_size(KObject);
void k_vector_set(KObject, k_cell_t, KObject);

// Intrinsics.
void k_in_add_vector(void);
void k_in_char_to_int(void);
void k_in_close(void);
void k_in_first(void);
void k_in_from_box();
void k_in_get(void);
void k_in_get_line(void);
void k_in_init(void);
void k_in_int_to_char(void);
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

////////////////////////////////////////////////////////////////////////////////
// Stack manipulation.

static inline void k_closure_drop(const size_t size) {
  for (size_t i = 0; i < size; ++i)
    k_object_release(k_closure[0][i]);
  k_mem_free(k_closure[0]);
  ++k_closure;
}

static inline void k_data_drop() {
  k_object_release(*k_data++);
}

static inline void k_locals_drop() {
  k_object_release(*k_locals++);
}

static inline KObject k_closure_get(const size_t i) {
  return k_closure[0][i];
}

static inline KObject k_locals_get(const size_t i) {
  return k_locals[i];
}

static inline void k_closure_push(KObject* const closure) {
  *--k_closure = closure;
}

static inline void k_call_push(const KCall call) {
  *--k_call = call;
}

static inline void k_locals_push(const KObject object) {
  *--k_locals = object;
}

static inline void k_data_push(const KObject object) {
  *--k_data = object;
}

static inline KObject k_data_pop() {
  return *k_data++;
}

static inline KCall k_call_pop() {
  return *k_call++;
}

////////////////////////////////////////////////////////////////////////////////
// Value creation.

static inline KObject k_bool_new(const k_bool_t value) {
  return (KObject) { .data = (KData) { .as_bool = !!value }, .type = K_BOOL };
}

static inline KObject k_char_new(const k_char_t value) {
  return (KObject) { .data = (KData) { .as_char = value }, .type = K_CHAR };
}

static inline KObject k_float_new(const k_float_t value) {
  return (KObject) { .data = (KData) { .as_float = value }, .type = K_FLOAT };
}

static inline KObject k_handle_new(const k_handle_t value) {
  return (KObject) { .data = (KData) { .as_handle = value }, .type = K_HANDLE };
}

static inline KObject k_int_new(const k_int_t value) {
  return (KObject) { .data = (KData) { .as_int = value }, .type = K_INT };
}

static inline KObject k_none_new() {
  return (KObject) { .data = (KData) { .as_int = 0 }, .type = K_NONE };
}

static inline KObject k_unit_new() {
  return (KObject) { .data = (KData) { .as_int = 0 }, .type = K_UNIT };
}

////////////////////////////////////////////////////////////////////////////////
// Intrinsics.

#define K_IN_ACT(LABEL, ...) \
  do { \
    k_data_push(k_activation_new(&&LABEL, __VA_ARGS__)); \
  } while (0)

#define K_IN_CALL(CALL, RETURN) \
  do { \
    k_call_push(((KCall) { \
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
    const KActivation* const activation = object.data.as_activation; \
    const size_t size = activation->end - activation->begin; \
    k_closure_push(k_mem_alloc(size, sizeof(KObject))); \
    for (size_t i = 0; i < size; ++i) \
      k_closure[0][i] = k_object_retain(activation->begin[i]); \
    k_call_push(((KCall) { \
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
      k_data_push(k_object_retain(choice.data.as_box->value)); \
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
      k_data_push(k_object_retain(choice.data.as_box->value)); \
      k_object_release(choice); \
      k_data_push(left); \
    } else if (choice.type == K_RIGHT) { \
      k_data_push(k_object_retain(choice.data.as_box->value)); \
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
    if (cond.data.as_bool) { \
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
    if (cond.data.as_bool) { \
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
      k_data_push(k_object_retain(option.data.as_box->value)); \
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
      k_data_push(k_object_retain(option.data.as_box->value)); \
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
    a->data.as_##TYPE = (a->data.as_##TYPE) OPERATION (b.data.as_##TYPE); \
  } while (0)

#define K_IN_RELATIONAL(TYPE, OPERATION) \
  do { \
    const KObject b = k_data_pop(); \
    KObject* const a = &k_data[0]; \
    assert(a->type == b.type); \
    a->data.as_##TYPE = (a->data.as_##TYPE) OPERATION (b.data.as_##TYPE); \
  } while (0)

#define K_IN_UNARY(TYPE, OPERATION) \
  do { \
    KObject* const a = &k_data[0]; \
    a->data.as_##TYPE = OPERATION (a->data.as_##TYPE);  \
  } while (0)

#endif
