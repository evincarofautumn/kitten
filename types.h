#ifndef TYPES_H
#define TYPES_H
#include <stdint.h>

/* A built-in word. */
typedef enum Word {
  /* Combinators. */
  WORD_DUP,
  WORD_SWAP,
  WORD_POP,
  WORD_QUOTE,
  WORD_COMPOSE,
  WORD_APPLY,
  /* Arithmetic. */
  WORD_ADD,
  WORD_SUB,
  WORD_MUL,
  WORD_DIV,
  WORD_MOD,
  /* Conditionals. */
  WORD_ISF,
  WORD_ISI,
  WORD_ISQ,
  WORD_ISW,
  WORD_EQ,
  WORD_NE,
  WORD_LT,
  WORD_GE,
  WORD_GT,
  WORD_LE,
  WORD_IF,
  /* I/O. */
  WORD_WRITE,
  /* Number of built-in words. */
  WORD_COUNT
} Word;

/* A built-in type. */
typedef enum Type {
  INTEGER, FLOAT, WORD, QUOTATION
} Type;

/* Forward declarations. */
struct Box;
union  Data;
struct Quotation;
struct Value;

/* Type aliases. */
typedef struct Box   *Boxed;
typedef struct Value *Unboxed;
typedef int64_t      Integer;
typedef double       Float;
typedef void         (*Implementation)(Boxed stack);

/* A vector of references. */
typedef struct Quotation {
  int   size;
  int   capacity;
  Boxed *data;
} Quotation;

/* A union of all built-in types. */
typedef union Data {
  Integer   as_integer;
  Float     as_float;
  Word      as_word;
  Quotation as_quotation;
} Data;

/* A typed, unboxed value. */
typedef struct Value {
  Type type;
  Data data;
} Value;

/* A reference-counted, boxed value. */
typedef struct Box {
  int   count;
  Value *value;
} Box;

/* *_alloc: Allocate unboxed values.
   *_new:   Create boxed values.
   *_unbox: Free and return boxed values. */

int       boxed_compare    (Boxed a, Boxed b);
Boxed     boxed_copy       (Boxed reference);
void      boxed_free       (Boxed reference);
Boxed     boxed_new        (Unboxed unboxed);
int       boxed_promote    (Boxed a, Boxed b, Boxed *da, Boxed *db);
Type      boxed_type       (Boxed reference);

Unboxed   float_alloc      (Float value);
Boxed     float_new        (Float value);
Float     float_unbox      (Boxed reference);
Float     float_value      (Boxed reference);

Unboxed   integer_alloc    (Integer value);
Boxed     integer_new      (Integer value);
Integer   integer_unbox    (Boxed reference);
Integer   integer_value    (Boxed reference);

int       is_integer       (Boxed reference);
int       is_float         (Boxed reference);
int       is_numeric       (Boxed reference);
int       is_quotation     (Boxed reference);
int       is_word          (Boxed reference);

Unboxed   quotation_alloc   (int size);
void      quotation_append  (Boxed destination, Boxed source);
void      quotation_apply   (Boxed destination, Boxed source);
void      quotation_clear   (Boxed quotation);
int       quotation_compare (Boxed a, Boxed b);
Boxed*    quotation_data    (Boxed quotation);
Boxed     quotation_new     (int size, ...);
void      quotation_push    (Boxed quotation, Boxed reference);
Boxed     quotation_pop     (Boxed quotation);
int       quotation_size    (Boxed quotation);
Boxed     quotation_top     (Boxed quotation);

void      unboxed_free     (Unboxed reference);
Unboxed   unboxed_alloc    (void);

Unboxed   word_alloc       (Word value);
void      word_apply       (Word word, Boxed stack);
Boxed     word_new         (Word value);
Word      word_unbox       (Boxed reference);
Word      word_value       (Boxed reference);

extern Implementation map[WORD_COUNT];

#endif
