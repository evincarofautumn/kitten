#include "Char.h"

#include <ostream>

Char::Char(const type value) : value(value) {}

std::shared_ptr<Value>
Char::copy() const {
  return std::make_shared<Char>(*this);
}

void
Char::write(std::ostream& stream) const {
  stream << char(value);
}
