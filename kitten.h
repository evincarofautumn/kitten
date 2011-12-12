#ifndef KITTEN_H
#define KITTEN_H
#include "types.h"

#define OPERATOR_DECLARATION(NAME) \
void kitten_##NAME(Boxed stack)

OPERATOR_DECLARATION(add);
OPERATOR_DECLARATION(div);
OPERATOR_DECLARATION(mod);
OPERATOR_DECLARATION(mul);
OPERATOR_DECLARATION(sub);

#undef OPERATOR_DECLARATION

void  kitten_apply   (Boxed stack);
void  kitten_compose (Boxed stack);
void  kitten_dup     (Boxed stack);
void  kitten_eq      (Boxed stack);
void  kitten_ge      (Boxed stack);
void  kitten_gt      (Boxed stack);
void  kitten_if      (Boxed stack);
void  kitten_isf     (Boxed stack);
void  kitten_isi     (Boxed stack);
void  kitten_isq     (Boxed stack);
void  kitten_isw     (Boxed stack);
void  kitten_le      (Boxed stack);
void  kitten_lt      (Boxed stack);
void  kitten_ne      (Boxed stack);
void  kitten_pop     (Boxed stack);
void  kitten_quote   (Boxed stack);
void  kitten_swap    (Boxed stack);
Boxed kitten_top     (Boxed stack);
void  kitten_write   (Boxed stack);

void  push           (Boxed stack, Boxed reference);

#endif
