#include "Handle.h"

#include <ostream>

Handle::Handle(const type value) : value(value) {}

MutableValuePtr
Handle::copy() const {
  return std::make_shared<Handle>(*this);
}

void
Handle::write(std::ostream& stream) const {
  stream << "<Handle " << value << ">";
}
