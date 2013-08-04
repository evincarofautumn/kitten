#include "Option.h"

#include <ostream>

Option::Option(const std::shared_ptr<const Value> value)
  : value(value) {}

std::shared_ptr<Value>
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
