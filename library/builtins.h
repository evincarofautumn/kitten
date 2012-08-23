#ifndef BUILTINS_H
#define BUILTINS_H

#define KITTEN_BUILTINS(INIT, LAST) \
  INIT(add)                         \
  INIT(apply)                       \
  INIT(compose)                     \
  INIT(div)                         \
  INIT(dup)                         \
  INIT(eq)                          \
  INIT(ge)                          \
  INIT(gt)                          \
  INIT(if)                          \
  INIT(isf)                         \
  INIT(isi)                         \
  INIT(isq)                         \
  INIT(isw)                         \
  INIT(le)                          \
  INIT(length)                      \
  INIT(lt)                          \
  INIT(mod)                         \
  INIT(mul)                         \
  INIT(ne)                          \
  INIT(pop)                         \
  INIT(putc)                        \
  INIT(quote)                       \
  INIT(sub)                         \
  INIT(swap)                        \
  INIT(trace)                       \
  LAST(write)

#endif
