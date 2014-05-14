#include "kitten.h"

#include <stdarg.h>
#include <stdlib.h>

KR* k_return;
KObject** k_closure;
KObject* k_data;
KObject* k_locals;

static KObject junk = { .data = K_JUNK, .type = K_JUNK };

static KObject* data_bottom;
static KObject* locals_bottom;
static KR* return_bottom;

static KObject* k_vector_begin(KObject);
static KObject* k_vector_end(KObject);

void k_init() {
#ifndef NDEBUG
  fprintf(stderr, "k_init()\n");
#endif

  const size_t CLOSURE_SIZE = 1024;
  k_closure = calloc(CLOSURE_SIZE, sizeof(KObject*));
  k_closure += CLOSURE_SIZE;

  const size_t RETURN_SIZE = 1024;
  k_return = calloc(RETURN_SIZE, sizeof(KR));
  k_return += RETURN_SIZE;
  return_bottom = k_return;

  const size_t DATA_SIZE = 1024;
  k_data = calloc(DATA_SIZE, sizeof(KObject));
  for (size_t i = 0; i < DATA_SIZE; ++i) {
    *k_data++ = junk;
  }
  data_bottom = k_data;

  const size_t LOCALS_SIZE = 1024;
  k_locals = calloc(LOCALS_SIZE, sizeof(KObject));
  for (size_t i = 0; i < LOCALS_SIZE; ++i) {
    *k_locals++ = junk;
  }
  locals_bottom = k_locals;

}

#if 0
static int is_boxed_type(const KType type) {
  return type >= K_BOXED;
}
#endif

#if 0
static k_cell_t* boxed_refs(KObject* const object) {
  return &(*((KBoxed**)&object->data))->refs;
}
#endif

KObject k_retain(const KObject object) {
#if 0
  if (is_boxed_type(object.type))
    ++(*((KBoxed**)&object.data))->refs;
#endif
  return object;
}
KObject k_release(KObject object) {

#if 0
  if (!is_boxed_type(object.type))
    return object;

  if (--*boxed_refs(&object) > 0)
    return object;

  // TODO Free members.

  free((KObject*)object.data);
#endif

  return object;

}

KObject k_activation(void* const function, const size_t size, ...) {
  KActivation* const activation = calloc(1, sizeof(KActivation));
  activation->refs = 1;
  activation->function = function;
  activation->begin = calloc(size, sizeof(KObject));
  activation->end = activation->begin + size;
  va_list args;
  va_start(args, size);
  for (size_t i = 0; i < size; ++i) {
    const KClosedName namespace = va_arg(args, KClosedName);
    assert(namespace == K_CLOSED || namespace == K_RECLOSED);
    const int index = va_arg(args, int);
    switch (namespace) {
    case K_CLOSED:
      activation->begin[i] = K_GET_LOCAL(index);
      break;
    case K_RECLOSED:
      activation->begin[i] = K_GET_CLOSURE(index);
      break;
    }
  }
  va_end(args);
  return (KObject) { .data = (k_cell_t)activation, .type = K_ACTIVATION };
}

KObject k_bool(const k_bool_t value) {
  return (KObject) { .data = !!value, .type = K_BOOL };
}

KObject k_char(const k_char_t value) {
  return (KObject) { .data = value, .type = K_CHAR };
}

KObject k_float(const k_float_t value) {
  return (KObject) { .data = *((k_cell_t*)&value), .type = K_FLOAT };
}

KObject k_from_box(const KObject value, const KType type) {
  assert(value.type == type);
  return ((KBoxed*)value.data)->as_box.value;
}

KObject k_handle(const k_handle_t value) {
  return (KObject) { .data = *((k_cell_t*)&value), .type = K_HANDLE };
}

KObject k_int(const k_int_t value) {
  return (KObject) { .data = value, .type = K_INT };
}

static KObject k_box(const KObject value, const KType type) {
  KBox* const data = calloc(1, sizeof(KBox));
  data->refs = 1;
  data->value = k_retain(value);
  const KObject result = {
    .data = (k_cell_t)data,
    .type = type
  };
  return result;
}

KObject k_left(const KObject value) {
  return k_box(value, K_LEFT);
}

KObject k_none() {
  return (KObject) { .data = 0, .type = K_NONE };
}

KObject k_pair(const KObject first, const KObject rest) {
  KPair* pair = calloc(1, sizeof(KPair));
  pair->first = k_retain(first);
  pair->rest = k_retain(rest);
  return (KObject) { .data = (k_cell_t)pair, .type = K_PAIR };
}

KObject k_right(const KObject value) {
  return k_box(value, K_RIGHT);
}

KObject k_some(const KObject value) {
  return k_box(value, K_SOME);
}

KObject k_unit() {
  return (KObject) { .data = 0, .type = K_UNIT };
}

KObject k_append_vector(
  const KObject a, const KObject b) {
  const size_t size = k_vector_size(a) + k_vector_size(b);
  const KObject vector = k_new_vector(size);
  KObject* from = k_vector_begin(a);
  KObject* to = k_vector_begin(vector);
  KObject* const a_end = k_vector_end(a);
  while (from != a_end)
    *to++ = k_retain(*from++);
  from = k_vector_begin(b);
  KObject* const b_end = k_vector_end(b);
  while (from != b_end)
    *to++ = k_retain(*from++);
  return vector;
}

/* Creates a new vector with uninitialized elements. */
KObject k_new_vector(const size_t size) {
  KVector* vector = calloc(1, sizeof(KVector));
  vector->begin = calloc(size, sizeof(KObject));
  vector->end = vector->begin + size;
  vector->capacity = vector->begin + size;
  return (KObject) {
    .data = (k_cell_t)vector,
    .type = K_VECTOR
  };
}

KObject k_vector(const size_t size, ...) {
  va_list args;
  va_start(args, size);
  KVector* vector = calloc(1, sizeof(KVector));
  vector->begin = calloc(size, sizeof(KObject));
  vector->end = vector->begin + size;
  vector->capacity = vector->begin + size;
  for (size_t i = 0; i < size; ++i) {
    vector->begin[i] = k_retain(va_arg(args, KObject));
  }
  va_end(args);
  return (KObject) {
    .data = (k_cell_t)vector,
    .type = K_VECTOR
  };
}

static KObject* k_vector_begin(const KObject object) {
  assert(object.type == K_VECTOR);
  return ((KVector*)object.data)->begin;
}

static KObject* k_vector_end(const KObject object) {
  assert(object.type == K_VECTOR);
  return ((KVector*)object.data)->end;
}

KObject k_vector_get(const KObject object, k_cell_t index) {
  assert(object.type == K_VECTOR);
  const KVector* const vector = (KVector*)object.data;
  return vector->begin[index];
}

void k_vector_set(const KObject object, k_cell_t index, const KObject value) {
  assert(object.type == K_VECTOR);
  const KVector* const vector = (KVector*)object.data;
  vector->begin[index] = value;
}

k_cell_t k_vector_size(const KObject object) {
  assert(object.type == K_VECTOR);
  const KVector* const vector = (const KVector*)object.data;
  return vector->end - vector->begin;
}

KObject k_make_vector(const size_t size) {
  KVector* vector = calloc(1, sizeof(KVector));
  vector->begin = calloc(size, sizeof(KObject));
  vector->end = vector->begin + size;
  vector->capacity = vector->begin + size;
  for (size_t i = 0; i < size; ++i) {
    vector->begin[i] = k_data[size - i - 1];
  }
  k_data += size;
  return (KObject) {
    .data = (k_cell_t)vector,
    .type = K_VECTOR
  };
}

#ifndef NDEBUG
static void dump_data() {
  fprintf(stderr, "[");
  for (KObject* p = k_data; p < data_bottom; ++p) {
    fprintf(stderr, "  %"PRId64"/%"PRId64"", p->data, p->type);
  }
  fprintf(stderr, "  ]\n");
}

static void dump_locals() {
  fprintf(stderr, "(");
  for (KObject* p = k_locals; p < locals_bottom; ++p) {
    fprintf(stderr, "  %"PRId64"/%"PRId64"", p->data, p->type);
  }
  fprintf(stderr, "  )\n");
}
#endif

void k_push_locals(const KObject object) {
  assert(object.type > K_UNBOXED && object.type < K_MAX_TYPE);
  *--k_locals = object;
#ifndef NDEBUG
  fprintf(stderr, "push locals\t");
  dump_locals();
#endif
}

void k_push_data(const KObject object) {
  assert(object.type > K_UNBOXED && object.type < K_MAX_TYPE);
  *--k_data = object;
#ifndef NDEBUG
  fprintf(stderr, "push data\t");
  dump_data();
#endif
}

KObject k_pop_data() {
  assert(k_data < data_bottom);
  const KObject x = k_data[0];
#ifndef NDEBUG
  k_data[0] = junk;
#endif
  ++k_data;
#ifndef NDEBUG
  fprintf(stderr, "pop data\t");
  dump_data();
#endif
  return x;
}

KObject k_pop_locals() {
  assert(k_locals < locals_bottom);
  const KObject x = k_locals[0];
#ifndef NDEBUG
  k_locals[0] = junk;
#endif
  ++k_locals;
#ifndef NDEBUG
  fprintf(stderr, "pop locals\t");
  dump_locals();
#endif
  return x;
}

KR k_pop_return() {
  assert(k_return < return_bottom);
  const KR x = k_return[0];
  ++k_return;
  return x;
}
