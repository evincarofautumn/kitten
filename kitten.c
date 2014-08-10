#include "kitten.h"

#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <stdlib.h>

KObject** k_closure;
KObject* k_data;
KObject* k_locals;
KCall* k_call;

static KObject junk = { .data = K_JUNK, .type = K_JUNK };

static KCall* call_bottom;
static KObject* data_bottom;
static KObject* locals_bottom;

static KCall* call_top;
static KObject* data_top;
static KObject* locals_top;
static KObject** closure_top;

static KObject k_box_new(KObject, KType);
static KObject* k_vector_begin(KObject);
static KObject* k_vector_end(KObject);

////////////////////////////////////////////////////////////////////////////////
// Runtime initialization.

void k_runtime_init() {

  const size_t CLOSURE_SIZE = 1024;
  k_closure = k_mem_alloc(CLOSURE_SIZE, sizeof(KObject*));
  closure_top = k_closure;
  k_closure += CLOSURE_SIZE;

  const size_t CALL_SIZE = 1024;
  k_call = k_mem_alloc(CALL_SIZE, sizeof(KCall));
  call_top = k_call;
  k_call += CALL_SIZE;
  call_bottom = k_call;

  const size_t DATA_SIZE = 1024;
  k_data = k_mem_alloc(DATA_SIZE, sizeof(KObject));
  data_top = k_data;
  for (size_t i = 0; i < DATA_SIZE; ++i) {
    *k_data++ = junk;
  }
  data_bottom = k_data;

  const size_t LOCALS_SIZE = 1024;
  k_locals = k_mem_alloc(LOCALS_SIZE, sizeof(KObject));
  locals_top = k_locals;
  for (size_t i = 0; i < LOCALS_SIZE; ++i) {
    *k_locals++ = junk;
  }
  locals_bottom = k_locals;

}

void k_runtime_quit() {
  k_mem_free(closure_top);
  while (k_data < data_bottom)
    k_data_drop();
  k_mem_free(data_top);
  while (k_locals < locals_bottom)
    k_locals_drop();
  k_mem_free(locals_top);
  k_mem_free(call_top);
}

////////////////////////////////////////////////////////////////////////////////
// Memory and reference counting.

void* k_mem_alloc(const size_t count, const size_t size) {
  void* allocated = calloc(count, size);
  assert(allocated);
  return allocated;
}

void k_mem_free(void* const pointer) {
  free(pointer);
}

KObject k_object_retain(const KObject object) {
  switch (object.type) {
  case K_ACTIVATION:
    ++((KActivation*)object.data)->refs;
    break;
  case K_LEFT:
  case K_RIGHT:
  case K_SOME:
    ++((KBox*)object.data)->refs;
    break;
  case K_PAIR:
    ++((KPair*)object.data)->refs;
    break;
  case K_VECTOR:
    ++((KVector*)object.data)->refs;
    break;
  }
  return object;
}

void k_object_release(const KObject object) {
  switch (object.type) {
  case K_ACTIVATION:
    {
      KActivation* const activation = (KActivation*)object.data;
      if (--activation->refs == 0) {
        for (KObject* object = activation->begin;
          object != activation->end; ++object) {
          k_object_release(*object);
        }
        k_mem_free(activation->begin);
        k_mem_free(activation);
      }
      break;
    }
  case K_LEFT:
  case K_RIGHT:
  case K_SOME:
    {
      KBox* const box = (KBox*)object.data;
      if (--box->refs == 0) {
        k_object_release(box->value);
        k_mem_free(box);
      }
      break;
    }
  case K_PAIR:
    {
      KPair* const pair = (KPair*)object.data;
      if (--pair->refs == 0) {
        k_object_release(pair->first);
        k_object_release(pair->rest);
        k_mem_free(pair);
      }
      break;
    }
  case K_VECTOR:
    {
      KVector* const vector = (KVector*)object.data;
      if (--vector->refs == 0) {
        const k_cell_t size = vector->end - vector->begin;
        for (k_cell_t i = 0; i < size; ++i)
          k_object_release(vector->begin[i]);
        k_mem_free(vector->begin);
        k_mem_free(vector);
      }
      break;
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
// Value creation.

KObject k_activation_new(void* const function, const size_t size, ...) {
  KActivation* const activation = k_mem_alloc(1, sizeof(KActivation));
  activation->refs = 1;
  activation->function = function;
  activation->begin = k_mem_alloc(size, sizeof(KObject));
  activation->end = activation->begin + size;
  va_list args;
  va_start(args, size);
  for (size_t i = 0; i < size; ++i) {
    const KClosedName namespace = va_arg(args, KClosedName);
    assert(namespace == K_CLOSED || namespace == K_RECLOSED);
    const int index = va_arg(args, int);
    switch (namespace) {
    case K_CLOSED:
      activation->begin[i] = k_object_retain(k_locals_get(index));
      break;
    case K_RECLOSED:
      activation->begin[i] = k_object_retain(k_closure_get(index));
      break;
    }
  }
  va_end(args);
  return (KObject) { .data = (k_cell_t)activation, .type = K_ACTIVATION };
}

KObject k_bool_new(const k_bool_t value) {
  return (KObject) { .data = !!value, .type = K_BOOL };
}

KObject k_char_new(const k_char_t value) {
  return (KObject) { .data = value, .type = K_CHAR };
}

KObject k_float_new(const k_float_t value) {
  return (KObject) { .data = *((k_cell_t*)&value), .type = K_FLOAT };
}

KObject k_handle_new(const k_handle_t value) {
  return (KObject) { .data = (k_cell_t)value, .type = K_HANDLE };
}

KObject k_int_new(const k_int_t value) {
  return (KObject) { .data = value, .type = K_INT };
}

KObject k_left_new(const KObject value) {
  return k_box_new(value, K_LEFT);
}

KObject k_none_new() {
  return (KObject) { .data = 0, .type = K_NONE };
}

KObject k_pair_new(const KObject first, const KObject rest) {
  KPair* pair = k_mem_alloc(1, sizeof(KPair));
  pair->refs = 1;
  pair->first = first;
  pair->rest = rest;
  return (KObject) { .data = (k_cell_t)pair, .type = K_PAIR };
}

KObject k_right_new(const KObject value) {
  return k_box_new(value, K_RIGHT);
}

KObject k_some_new(const KObject value) {
  return k_box_new(value, K_SOME);
}

KObject k_unit_new() {
  return (KObject) { .data = 0, .type = K_UNIT };
}

// Creates a new vector with uninitialized elements.
KObject k_vector_new(const size_t size) {
  KVector* const vector = k_mem_alloc(1, sizeof(KVector));
  vector->refs = 1;
  vector->begin = k_mem_alloc(size, sizeof(KObject));
  vector->end = vector->begin + size;
  vector->capacity = vector->begin + size;
  return (KObject) {
    .data = (k_cell_t)vector,
    .type = K_VECTOR
  };
}

////////////////////////////////////////////////////////////////////////////////
// Box operations.

KObject k_box_get(const KObject value, const KType type) {
  assert(value.type == type);
  return ((KBoxed*)value.data)->as_box.value;
}

static KObject k_box_new(const KObject value, const KType type) {
  KBox* const data = k_mem_alloc(1, sizeof(KBox));
  data->refs = 1;
  data->value = value;
  return (KObject) {
    .data = (k_cell_t)data,
    .type = type
  };
}

////////////////////////////////////////////////////////////////////////////////
// Pair operations.

KObject k_pair_first(const KObject pair) {
  assert(pair.type == K_PAIR);
  return ((KPair*)pair.data)->first;
}

KObject k_pair_rest(const KObject pair) {
  assert(pair.type == K_PAIR);
  return ((KPair*)pair.data)->rest;
}

////////////////////////////////////////////////////////////////////////////////
// Vector operations.

KObject k_vector_append(const KObject a, const KObject b) {
  const k_cell_t size = k_vector_size(a) + k_vector_size(b);
  const KObject vector = k_vector_new(size);
  const KObject* from = k_vector_begin(a);
  KObject* to = k_vector_begin(vector);
  const KObject* const a_end = k_vector_end(a);
  while (from != a_end)
    *to++ = k_object_retain(*from++);
  from = k_vector_begin(b);
  const KObject* const b_end = k_vector_end(b);
  while (from != b_end)
    *to++ = k_object_retain(*from++);
  assert(to == k_vector_end(vector));
  return vector;
}

KObject k_vector(const size_t size, ...) {
  va_list args;
  va_start(args, size);
  KVector* const vector = k_mem_alloc(1, sizeof(KVector));
  vector->refs = 1;
  vector->begin = k_mem_alloc(size, sizeof(KObject));
  vector->end = vector->begin + size;
  vector->capacity = vector->begin + size;
  for (size_t i = 0; i < size; ++i) {
    vector->begin[i] = va_arg(args, KObject);
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
  assert(k_vector_size(object) > index);
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

////////////////////////////////////////////////////////////////////////////////
// Intrinsics.

void k_in_add_vector() {
  KObject b = k_data_pop();
  KObject a = k_data_pop();
  assert(a.type == K_VECTOR);
  assert(b.type == K_VECTOR);
  k_data_push(k_vector_append(a, b));
  k_object_release(a);
  k_object_release(b);
}

void k_in_close() {
  KObject handle = k_data_pop();
  assert(handle.type == K_HANDLE);
  fclose((FILE*)handle.data);
}

void k_in_first() {
  const KObject a = k_data_pop();
  assert(a.type == K_PAIR);
  k_data_push(k_object_retain(k_pair_first(a)));
  k_object_release(a);
}

void k_in_from_box(const KType type) {
  const KObject a = k_data_pop();
  k_data_push(k_object_retain(k_box_get(a, type)));
  k_object_release(a);
}

void k_in_get() {
  const KObject index = k_data_pop();
  assert(index.type == K_INT);
  const KObject vector = k_data_pop();
  assert(vector.type == K_VECTOR);
  const k_int_t i = (k_int_t)index.data;
  const k_cell_t size = k_vector_size(vector);
  k_data_push(i < 0 || i >= size
    ? k_none_new() : k_some_new(k_object_retain(k_vector_get(vector, i))));
  k_object_release(vector);
}

void k_in_get_line() {
  KObject handle = k_data_pop();
  assert(handle.type == K_HANDLE);
  size_t length = 1;
  char line[1024];
  if (fgets(line, 1024, (k_handle_t)handle.data))
    length = strlen(line);
  const KObject string = k_vector_new(length - 1);
  for (size_t i = 0; i < length - 1; ++i)
    k_vector_set(string, i, k_char_new(line[i]));
  k_data_push(string);
}

void k_in_init() {
  const KObject vector = k_data_pop();
  assert(vector.type == K_VECTOR);
  const k_cell_t size = k_vector_size(vector);
  const KObject result = k_vector_new(size == 0 ? 0 : size - 1);
  for (k_cell_t i = 0; i + 1 < size; ++i)
    k_vector_set(result, i, k_object_retain(k_vector_get(vector, i)));
  k_data_push(result);
  k_object_release(vector);
}

void k_in_left() {
  const KObject left = k_left_new(k_data_pop());
  k_data_push(left);
}

void k_in_length() {
  const KObject vector = k_data_pop();
  assert(vector.type == K_VECTOR);
  k_data_push(k_int_new(k_vector_size(vector)));
  k_object_release(vector);
}

void k_in_make_vector(const size_t size) {
  KVector* const vector = k_mem_alloc(1, sizeof(KVector));
  vector->refs = 1;
  vector->begin = k_mem_alloc(size, sizeof(KObject));
  vector->end = vector->begin + size;
  vector->capacity = vector->begin + size;
  // Moving from stack into vector; no retain/release needed.
  for (size_t i = 0; i < size; ++i)
    vector->begin[i] = k_data[size - i - 1];
  k_data += size;
  k_data_push((KObject) {
    .data = (k_cell_t)vector,
    .type = K_VECTOR
  });
}

void k_in_mod_float() {
  const KObject b = k_data_pop();
  const KObject a = k_data_pop();
  assert(a.type == b.type);
  k_data_push(k_float_new(fmod((k_float_t)a.data, (k_float_t)b.data)));
}

void k_in_pair() {
  const KObject b = k_data_pop();
  const KObject a = k_data_pop();
  k_data_push(k_pair_new(a, b));
}

void k_in_print() {
  const KObject handle = k_data_pop();
  assert(handle.type == K_HANDLE);
  const KObject string = k_data_pop();
  assert(string.type == K_VECTOR);
  for (KObject* character = ((KVector*)string.data)->begin;
    character != ((KVector*)string.data)->end; ++character) {
    fputc(character->data, (FILE*)handle.data);
  }
  k_object_release(string);
}

void k_in_rest() {
  const KObject a = k_data_pop();
  assert(a.type == K_PAIR);
  k_data_push(k_object_retain(k_pair_rest(a)));
  k_object_release(a);
}

void k_in_right() {
  const KObject right = k_right_new(k_data_pop());
  k_data_push(right);
}

void k_in_set() {
  const KObject value = k_data_pop();
  const KObject index = k_data_pop();
  assert(index.type == K_INT);
  const KObject vector = k_data_pop();
  assert(vector.type == K_VECTOR);
  const k_cell_t size = k_vector_size(vector);
  const KObject result = k_vector_new(size);
  for (k_cell_t i = 0; i < size; ++i) {
    if (i == index.data) {
      k_vector_set(result, i, value);
      continue;
    }
    k_vector_set(result, i, k_object_retain(k_vector_get(vector, i)));
  }
  k_object_release(vector);
  k_data_push(result);
}

void k_in_show_float() {
  const KObject x = k_data_pop();
  assert(x.type == K_FLOAT);
  char buffer[
    1  // sign
    + (DBL_MAX_10_EXP + 1)  // decimal digits
    + 1  // dot
    + 6  // default precision
    + 1  // null
  ];
  int length = 0;
  snprintf(buffer, sizeof(buffer),
    "%f%n", *(k_float_t*)(&x.data), &length);
  const KObject string = k_vector_new(length);
  for (size_t i = 0; i < length; ++i)
    k_vector_set(string, i, k_char_new(buffer[i]));
  k_data_push(string);
}

void k_in_show_int() {
  const KObject x = k_data_pop();
  assert(x.type == K_INT);
  char buffer[20] = {0};
  int length = 0;
  snprintf(buffer, sizeof(buffer),
    "%"PRId64"%n", (k_int_t)x.data, &length);
  const KObject string = k_vector_new(length);
  for (size_t i = 0; i < length; ++i)
    k_vector_set(string, i, k_char_new(buffer[i]));
  k_data_push(string);
}

void k_in_some() {
  KObject some = k_some_new(k_data_pop());
  k_data_push(some);
}

void k_in_tail() {
  const KObject vector = k_data_pop();
  assert(vector.type == K_VECTOR);
  const k_cell_t size = k_vector_size(vector);
  const KObject result = k_vector_new(size == 0 ? 0 : size - 1);
  for (k_cell_t i = 0; i + 1 < size; ++i)
    k_vector_set(result, i, k_object_retain(k_vector_get(vector, i + 1)));
  k_data_push(result);
  k_object_release(vector);
}

////////////////////////////////////////////////////////////////////////////////
// Stack manipulation.

void k_closure_drop(const size_t size) {
  for (size_t i = 0; i < size; ++i)
    k_object_release(k_closure[0][i]);
  k_mem_free(k_closure[0]);
  ++k_closure;
}

void k_data_drop() {
  k_object_release(k_data[0]);
  k_data[0] = junk;
  ++k_data;
}

void k_locals_drop() {
  k_object_release(k_locals[0]);
  k_locals[0] = junk;
  ++k_locals;
}

KObject k_closure_get(const k_cell_t i) {
  return k_closure[0][i];
}

KObject k_locals_get(const k_cell_t i) {
  return k_locals[i];
}

void k_closure_push(KObject* const object) {
  *--k_closure = object;
}

void k_call_push(KCall call) {
  *--k_call = call;
}

void k_locals_push(const KObject object) {
  assert(object.type > K_UNBOXED && object.type < K_MAX_TYPE);
  *--k_locals = object;
#ifndef NDEBUG
  fprintf(stderr, "push locals\t");
  dump_locals();
#endif
}

void k_data_push(const KObject object) {
  assert(object.type > K_UNBOXED && object.type < K_MAX_TYPE);
  *--k_data = object;
#ifndef NDEBUG
  fprintf(stderr, "push data\t");
  dump_data();
#endif
}

KObject k_data_pop() {
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

KCall k_call_pop() {
  assert(k_call < call_bottom);
  const KCall x = k_call[0];
  ++k_call;
  return x;
}

////////////////////////////////////////////////////////////////////////////////
// Debugging.

#ifndef NDEBUG
static void dump_object(const KObject object) {
  if (object.type == K_JUNK) {
    fprintf(stderr, "<junk>");
  } else if (object.type == K_BOOL) {
    fprintf(stderr, "%s", object.data ? "true" : "false");
  } else if (object.type == K_CHAR) {
    fprintf(stderr, "'%c'", (char)(object.data));
  } else if (object.type == K_FLOAT) {
    fprintf(stderr, "%f", *(const double*)(&object.data));
  } else if (object.type == K_INT) {
    fprintf(stderr, "%"PRId64"", object.data);
  } else if (object.type == K_NONE) {
    fprintf(stderr, "none");
  } else if (object.type == K_UNIT) {
    fprintf(stderr, "()");
  } else if (object.type == K_ACTIVATION) {
    fprintf(stderr, "<act>");
  } else if (object.type == K_HANDLE) {
    fprintf(stderr, "<handle>");
  } else if (object.type == K_LEFT) {
    fprintf(stderr, "(");
    dump_object(k_box_get(object, K_LEFT));
    fprintf(stderr, " left)");
  } else if (object.type == K_PAIR) {
    fprintf(stderr, "(");
    dump_object(k_pair_first(object));
    fprintf(stderr, " ");
    dump_object(k_pair_rest(object));
    fprintf(stderr, " pair)");
  } else if (object.type == K_RIGHT) {
    fprintf(stderr, "(");
    dump_object(k_box_get(object, K_RIGHT));
    fprintf(stderr, " right)");
  } else if (object.type == K_SOME) {
    fprintf(stderr, "(");
    dump_object(k_box_get(object, K_SOME));
    fprintf(stderr, " some)");
  } else if (object.type == K_VECTOR) {
    if (k_vector_size(object) > 0 && k_vector_get(object, 0).type == K_CHAR) {
      fputc('"', stderr);
      for (const KObject* c = k_vector_begin(object);
           c != k_vector_end(object); ++c) {
        fputc(isprint(c->data) ? c->data : '.', stderr);
      }
      fputc('"', stderr);
    } else {
      fprintf(stderr, "[");
      for (k_cell_t i = 0; i < k_vector_size(object); ++i) {
        fprintf(stderr, " ");
        dump_object(k_vector_get(object, i));
      }
      fprintf(stderr, " ]");
    }
  } else {
    fprintf(stderr, "<unknown:%"PRId64">", object.type);
  }
}

static void dump_data() {
  for (const KObject* p = data_bottom - 1; p >= k_data; --p) {
    fprintf(stderr, "  ");
    dump_object(*p);
  }
  fputc('\n', stderr);
}

static void dump_locals() {
  fprintf(stderr, "(");
  for (const KObject* p = k_locals; p < locals_bottom; ++p) {
    fprintf(stderr, "  ");
    dump_object(*p);
  }
  fprintf(stderr, "  )\n");
}
#endif
