#include "Vector.h"

#include "Char.h"

#include <ostream>

Vector::Vector(const type& value)
  : value(value) {}

MutableValuePtr
Vector::copy() const {
  const auto result = std::make_shared<Vector>();
  for (const auto& element : value)
    result->value.push_back(element->copy());
  return result;
}

void
Vector::write(std::ostream& stream) const {
  if (!value.empty() and typeid(*value[0]) == typeid(Char)) {
    stream << '"';
    for (const auto& element : value)
      stream << char(element->value<Char>());
    stream << '"';
  } else {
    stream << "[";
    for (const auto& element : value)
      stream << *element << ", ";
    stream << "]";
  }
}
