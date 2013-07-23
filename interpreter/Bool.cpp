#include "Bool.h"

#include <iomanip>
#include <ostream>

Bool::Bool(const type value)
  : value(value) {}

std::shared_ptr<Value>
Bool::copy() const {
  return std::make_shared<Bool>(*this);
}

void
Bool::write(std::ostream& stream) const {
  stream << std::boolalpha << value;
}
