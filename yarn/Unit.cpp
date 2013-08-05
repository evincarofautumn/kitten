#include "Unit.h"

#include <ostream>

MutableValuePtr
Unit::copy() const {
  return std::make_shared<Unit>(*this);
}

void
Unit::write(std::ostream& stream) const {
  stream << "()";
}
