#include "Float.h"

#include <ostream>

Float::Float(const type value) : value(value) {}

MutableValuePtr
Float::copy() const {
  return std::make_shared<Float>(*this);
}

void
Float::write(std::ostream& stream) const {
  stream << value;
}
