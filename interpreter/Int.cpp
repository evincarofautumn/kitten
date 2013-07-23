#include "Int.h"

#include <ostream>

Int::Int(const type value) : value(value) {}

std::shared_ptr<Value>
Int::copy() const {
  return std::make_shared<Int>(*this);
}

void
Int::write(std::ostream& stream) const {
  stream << value;
}
