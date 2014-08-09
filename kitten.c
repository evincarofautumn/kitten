#include "kitten.h"

#include <ctype.h>
#include <gc/gc.h>
#include <math.h>
#include <stdarg.h>
#include <stdlib.h>

KObject** k_closure;
KObject* k_data;
KObject* k_locals;
KR* k_return;

static KObject junk = { .data = K_JUNK, .type = K_JUNK };

static KObject* data_bottom;
static KObject* locals_bottom;
static KR* return_bottom;

static KObject* k_vector_begin(KObject);
static KObject* k_vector_end(KObject);

void* k_alloc(const size_t count, const size_t size) {
  void* allocated = calloc(count, size);
  assert(allocated);
  return allocated;
}

void k_free(void* const pointer) {
  free(pointer);
}

KObject k_retain(const KObject object) {
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

void k_release(const KObject object) {
  switch (object.type) {
  case K_ACTIVATION:
    {
      KActivation* const activation = (KActivation*)object.data;
      if (--activation->refs == 0) {
        for (KObject* object = activation->begin;
          object != activation->end; ++object) {
          k_release(*object);
        }
        k_free(activation->begin);
        k_free(activation);
      }
      break;
    }
  case K_LEFT:
  case K_RIGHT:
  case K_SOME:
    {
      KBox* const box = (KBox*)object.data;
      if (--box->refs == 0) {
        k_release(box->value);
        k_free(box);
      }
      break;
    }
  case K_PAIR:
    {
      KPair* const pair = (KPair*)object.data;
      if (--pair->refs == 0) {
        k_release(pair->first);
        k_release(pair->rest);
        k_free(pair);
      }
      break;
    }
  case K_VECTOR:
    {
      KVector* const vector = (KVector*)object.data;
      if (--vector->refs == 0) {
        const k_cell_t size = vector->end - vector->begin;
        for (k_cell_t i = 0; i < size; ++i)
          k_release(vector->begin[i]);
        k_free(vector->begin);
        k_free(vector);
      }
      break;
    }
  }
}

void k_runtime_init() {

  const size_t CLOSURE_SIZE = 1024;
  k_closure = k_alloc(CLOSURE_SIZE, sizeof(KObject*));
  k_closure += CLOSURE_SIZE;

  const size_t RETURN_SIZE = 1024;
  k_return = k_alloc(RETURN_SIZE, sizeof(KR));
  k_return += RETURN_SIZE;
  return_bottom = k_return;

  const size_t DATA_SIZE = 1024;
  k_data = k_alloc(DATA_SIZE, sizeof(KObject));
  for (size_t i = 0; i < DATA_SIZE; ++i) {
    *k_data++ = junk;
  }
  data_bottom = k_data;

  const size_t LOCALS_SIZE = 1024;
  k_locals = k_alloc(LOCALS_SIZE, sizeof(KObject));
  for (size_t i = 0; i < LOCALS_SIZE; ++i) {
    *k_locals++ = junk;
  }
  locals_bottom = k_locals;

}

void k_runtime_quit() {
  while (k_data < data_bottom)
    k_drop_data();
  while (k_locals < locals_bottom)
    k_drop_locals();
}

void k_drop_closure(const size_t size) {
  for (size_t i = 0; i < size; ++i)
    k_release(k_closure[0][i]);
  ++k_closure;
}

void k_drop_data() {
  k_release(k_data[0]);
  k_data[0] = junk;
  ++k_data;
}

void k_drop_locals() {
  k_release(k_locals[0]);
  k_locals[0] = junk;
  ++k_locals;
}

KObject k_get_closure(const k_cell_t i) {
  return k_closure[0][i];
}

KObject k_get_local(const k_cell_t i) {
  return k_locals[i];
}

void k_push_closure(KObject* const object) {
  *--k_closure = object;
}

void k_push_return(KR call) {
  *--k_return = call;
}

void k_get_line() {
  KObject handle = k_pop_data();
  assert(handle.type == K_HANDLE);
  size_t length = 1;
  char line[1024];
  if (fgets(line, 1024, *(k_handle_t*)(&handle.data)))
    length = strlen(line);
  KObject string = k_new_vector(length - 1);
  for (size_t i = 0; i < length - 1; ++i)
    k_vector_set(string, i, k_new_char(line[i]));
  k_push_data(string);
}

void k_make_vector(const size_t size) {
  KVector* const vector = k_alloc(1, sizeof(KVector));
  vector->refs = 1;
  vector->begin = k_alloc(size, sizeof(KObject));
  vector->end = vector->begin + size;
  vector->capacity = vector->begin + size;
  // Moving from stack into vector; no retain/release needed.
  for (size_t i = 0; i < size; ++i)
    vector->begin[i] = k_data[size - i - 1];
  k_data += size;
  k_push_data((KObject) {
    .data = (k_cell_t)vector,
    .type = K_VECTOR
  });
}

void k_add_vector() {
  KObject b = k_pop_data();
  KObject a = k_pop_data();
  assert(a.type == K_VECTOR);
  assert(b.type == K_VECTOR);
  k_push_data(k_append_vector(a, b));
  k_release(a);
  k_release(b);
}

void k_close() {
  KObject handle = k_pop_data();
  assert(handle.type == K_HANDLE);
  fclose((FILE*)handle.data);
}

void k_first() {
  const KObject pair = k_pop_data();
  assert(pair.type == K_PAIR);
  k_push_data(k_retain(((KPair*)pair.data)->first));
  k_release(pair);
}

void k_get() {
  const KObject index = k_pop_data();
  assert(index.type == K_INT);
  const KObject vector = k_pop_data();
  assert(vector.type == K_VECTOR);
  const k_int_t i = (k_int_t)index.data;
  const k_cell_t size = k_vector_size(vector);
  k_push_data(i < 0 || i >= size
    ? k_new_none() : k_new_some(k_retain(k_vector_get(vector, i))));
  k_release(vector);
}

void k_init() {
  const KObject vector = k_pop_data();
  assert(vector.type == K_VECTOR);
  const k_cell_t size = k_vector_size(vector);
  const KObject result = k_new_vector(size == 0 ? 0 : size - 1);
  for (k_cell_t i = 0; i + 1 < size; ++i)
    k_vector_set(result, i, k_retain(k_vector_get(vector, i)));
  k_push_data(result);
  k_release(vector);
}

void k_left() {
  const KObject left = k_new_left(k_pop_data());
  k_push_data(left);
}

void k_mod_float() {
  const KObject b = k_pop_data();
  const KObject a = k_pop_data();
  assert(a.type == b.type);
  k_push_data(k_new_float(fmod((k_float_t)a.data, (k_float_t)b.data)));
}

void k_right() {
  const KObject right = k_new_right(k_pop_data());
  k_push_data(right);
}

void k_rest() {
  const KObject pair = k_pop_data();
  assert(pair.type == K_PAIR);
  k_push_data(k_retain(((KPair*)pair.data)->rest));
  k_release(pair);
}

void k_length() {
  const KObject vector = k_pop_data();
  assert(vector.type == K_VECTOR);
  k_push_data(k_new_int(k_vector_size(vector)));
  k_release(vector);
}

void k_pair() {
  const KObject b = k_pop_data();
  const KObject a = k_pop_data();
  k_push_data(k_new_pair(a, b));
}

/* TODO Copy on write. */
void k_set() {
  const KObject value = k_pop_data();
  const KObject index = k_pop_data();
  assert(index.type == K_INT);
  const KObject vector = k_pop_data();
  assert(vector.type == K_VECTOR);
  const k_cell_t size = k_vector_size(vector);
  const KObject result = k_new_vector(size);
  for (k_cell_t i = 0; i < size; ++i) {
    if (i == index.data) {
      k_vector_set(result, i, value);
      continue;
    }
    k_vector_set(result, i, k_retain(k_vector_get(vector, i)));
  }
  k_release(vector);
  k_push_data(result);
}

void k_some() {
  KObject some = k_new_some(k_pop_data());
  k_push_data(some);
}

void k_tail() {
  const KObject vector = k_pop_data();
  assert(vector.type == K_VECTOR);
  const k_cell_t size = k_vector_size(vector);
  const KObject result = k_new_vector(size == 0 ? 0 : size - 1);
  for (k_cell_t i = 0; i + 1 < size; ++i)
    k_vector_set(result, i, k_retain(k_vector_get(vector, i + 1)));
  k_push_data(result);
  k_release(vector);
}

void k_from_box(const KType type) {
  const KObject a = k_pop_data();
  k_push_data(k_retain(k_box_get(a, type)));
  k_release(a);
}

void k_print() {
  const KObject handle = k_pop_data();
  assert(handle.type == K_HANDLE);
  const KObject string = k_pop_data();
  assert(string.type == K_VECTOR);
  for (KObject* character = ((KVector*)string.data)->begin;
    character != ((KVector*)string.data)->end; ++character) {
    fputc(character->data, (FILE*)handle.data);
  }
  k_release(string);
}

void k_show_float() {
  const KObject x = k_pop_data();
  assert(x.type == K_FLOAT);
  char buffer[
    1 /* sign */
    + (DBL_MAX_10_EXP + 1) /* decimal digits */
    + 1 /* dot */
    + 6 /* default precision */
    + 1 /* null */
  ];
  int length = 0;
  snprintf(buffer, sizeof(buffer),
    "%f%n", *(k_float_t*)(&x.data), &length);
  const KObject string = k_new_vector(length);
  for (size_t i = 0; i < length; ++i)
    k_vector_set(string, i, k_new_char(buffer[i]));
  k_push_data(string);
}

void k_show_int() {
  const KObject x = k_pop_data();
  assert(x.type == K_INT);
  char buffer[20] = {0};
  int length = 0;
  snprintf(buffer, sizeof(buffer),
    "%"PRId64"%n", (k_int_t)x.data, &length);
  const KObject string = k_new_vector(length);
  for (size_t i = 0; i < length; ++i)
    k_vector_set(string, i, k_new_char(buffer[i]));
  k_push_data(string);
}

KObject k_new_activation(void* const function, const size_t size, ...) {
  KActivation* const activation = k_alloc(1, sizeof(KActivation));
  activation->refs = 1;
  activation->function = function;
  activation->begin = k_alloc(size, sizeof(KObject));
  activation->end = activation->begin + size;
  va_list args;
  va_start(args, size);
  for (size_t i = 0; i < size; ++i) {
    const KClosedName namespace = va_arg(args, KClosedName);
    assert(namespace == K_CLOSED || namespace == K_RECLOSED);
    const int index = va_arg(args, int);
    switch (namespace) {
    case K_CLOSED:
      activation->begin[i] = k_retain(k_get_local(index));
      break;
    case K_RECLOSED:
      activation->begin[i] = k_retain(k_get_closure(index));
      break;
    }
  }
  va_end(args);
  return (KObject) { .data = (k_cell_t)activation, .type = K_ACTIVATION };
}

KObject k_new_bool(const k_bool_t value) {
  return (KObject) { .data = !!value, .type = K_BOOL };
}

KObject k_new_char(const k_char_t value) {
  return (KObject) { .data = value, .type = K_CHAR };
}

KObject k_new_float(const k_float_t value) {
  return (KObject) { .data = *((k_cell_t*)&value), .type = K_FLOAT };
}

KObject k_box_get(const KObject value, const KType type) {
  assert(value.type == type);
  return ((KBoxed*)value.data)->as_box.value;
}

KObject k_new_handle(const k_handle_t value) {
  return (KObject) { .data = (k_cell_t)value, .type = K_HANDLE };
}

KObject k_new_int(const k_int_t value) {
  return (KObject) { .data = value, .type = K_INT };
}

static KObject k_new_box(const KObject value, const KType type) {
  KBox* const data = k_alloc(1, sizeof(KBox));
  data->refs = 1;
  data->value = value;
  return (KObject) {
    .data = (k_cell_t)data,
    .type = type
  };
}

KObject k_new_left(const KObject value) {
  return k_new_box(value, K_LEFT);
}

KObject k_new_none() {
  return (KObject) { .data = 0, .type = K_NONE };
}

KObject k_new_pair(const KObject first, const KObject rest) {
  KPair* pair = k_alloc(1, sizeof(KPair));
  pair->refs = 1;
  pair->first = first;
  pair->rest = rest;
  return (KObject) { .data = (k_cell_t)pair, .type = K_PAIR };
}

KObject k_new_right(const KObject value) {
  return k_new_box(value, K_RIGHT);
}

KObject k_new_some(const KObject value) {
  return k_new_box(value, K_SOME);
}

KObject k_new_unit() {
  return (KObject) { .data = 0, .type = K_UNIT };
}

KObject k_append_vector(
  const KObject a, const KObject b) {
  const k_cell_t size = k_vector_size(a) + k_vector_size(b);
  const KObject vector = k_new_vector(size);
  const KObject* from = k_vector_begin(a);
  KObject* to = k_vector_begin(vector);
  const KObject* const a_end = k_vector_end(a);
  while (from != a_end)
    *to++ = k_retain(*from++);
  from = k_vector_begin(b);
  const KObject* const b_end = k_vector_end(b);
  while (from != b_end)
    *to++ = k_retain(*from++);
  assert(to == k_vector_end(vector));
  return vector;
}

/* Creates a new vector with uninitialized elements. */
KObject k_new_vector(const size_t size) {
  KVector* const vector = k_alloc(1, sizeof(KVector));
  vector->refs = 1;
  vector->begin = k_alloc(size, sizeof(KObject));
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
  KVector* const vector = k_alloc(1, sizeof(KVector));
  vector->refs = 1;
  vector->begin = k_alloc(size, sizeof(KObject));
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
    dump_object(((const KBox*)object.data)->value);
    fprintf(stderr, " left)");
  } else if (object.type == K_PAIR) {
    fprintf(stderr, "(");
    dump_object(((const KPair*)object.data)->first);
    fprintf(stderr, " ");
    dump_object(((const KPair*)object.data)->rest);
    fprintf(stderr, " pair)");
  } else if (object.type == K_RIGHT) {
    fprintf(stderr, "(");
    dump_object(((const KBox*)object.data)->value);
    fprintf(stderr, " right)");
  } else if (object.type == K_SOME) {
    fprintf(stderr, "(");
    dump_object(((const KBox*)object.data)->value);
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
