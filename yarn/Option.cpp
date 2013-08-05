#include "Option.h"

#include <ostream>

Option::Option(const ValuePtr value)
  : value(value) {}

MutableValuePtr
Option::copy() const {
  return std::make_shared<Option>(*this);
}

void
Option::write(std::ostream& stream) const {
  if (value) {
    stream << "some " << *value;
  } else {
    stream << "none";
  }
}
