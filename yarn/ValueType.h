#ifndef VALUETYPE_H_59FB02FA_5D97_4952_BA46_D99FC0F3E159
#define VALUETYPE_H_59FB02FA_5D97_4952_BA46_D99FC0F3E159

#include <string>
#include <unordered_map>

enum class ValueType {
  BOOL,
  CHAR,
  FLOAT,
  HANDLE,
  INT,
  VECTOR,
  PAIR,
  UNIT,
};

extern const std::unordered_map<std::string, ValueType> VALUE_TYPE;

#endif
