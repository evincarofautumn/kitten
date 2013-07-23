#ifndef BUILTINID_H_0E27668D_C239_444D_81CE_820988766C19
#define BUILTINID_H_0E27668D_C239_444D_81CE_820988766C19

#include <string>
#include <unordered_map>

enum class BuiltinId {
  AddFloat,       // 0
  AddInt,         // 1
  AddVector,      // 2
  AndBool,        // 3
  AndInt,         // 4
  Apply,          // 5
  CharToInt,      // 6
  Close,          // 7
  DivFloat,       // 8
  DivInt,         // 9
  EqFloat,        // 10
  EqInt,          // 11
  Exit,           // 12
  First,          // 13
  FromLeft,       // 14
  FromRight,      // 15
  FromSome,       // 16
  GeFloat,        // 17
  GeInt,          // 18
  Get,            // 19
  GetLine,        // 20
  GtFloat,        // 21
  GtInt,          // 22
  Impure,         // 23
  Init,           // 24
  LeFloat,        // 25
  LeInt,          // 26
  Left,           // 27
  Length,         // 28
  LtFloat,        // 29
  LtInt,          // 30
  ModFloat,       // 31
  ModInt,         // 32
  MulFloat,       // 33
  MulInt,         // 34
  NeFloat,        // 35
  NeInt,          // 36
  NegFloat,       // 37
  NegInt,         // 38
  None,           // 39
  NotBool,        // 40
  NotInt,         // 41
  OpenIn,         // 42
  OpenOut,        // 43
  OrBool,         // 44
  OrInt,          // 45
  Pair,           // 46
  Print,          // 47
  Rest,           // 48
  Right,          // 49
  Set,            // 50
  ShowFloat,      // 51
  ShowInt,        // 52
  Some,           // 53
  Stderr,         // 54
  Stdin,          // 55
  Stdout,         // 56
  SubFloat,       // 57
  SubInt,         // 58
  Tail,           // 59
  UnsafePurify11, // 60
  XorBool,        // 62
  XorInt,         // 63
};

extern const std::unordered_map<std::string, BuiltinId> BUILTIN;

#endif
