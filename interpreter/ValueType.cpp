#include "ValueType.h"

const std::unordered_map<std::string, ValueType>
VALUE_TYPE {
  std::make_pair("bool", ValueType::BOOL),
  std::make_pair("char", ValueType::CHAR),
  std::make_pair("float", ValueType::FLOAT),
  std::make_pair("handle", ValueType::HANDLE),
  std::make_pair("int", ValueType::INT),
  std::make_pair("vector", ValueType::VECTOR),
  std::make_pair("pair", ValueType::PAIR),
  std::make_pair("unit", ValueType::UNIT),
};
