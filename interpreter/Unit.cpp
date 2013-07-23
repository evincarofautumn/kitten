#include "Unit.h"

#include <ostream>

std::shared_ptr<Value>
Unit::copy() const {
  return std::make_shared<Unit>(*this);
}

void
Unit::write(std::ostream& stream) const {
  stream << "()";
}
