#include "Handle.h"

#include <iostream>

ValuePtr Handle::stderr(new Handle(&std::cerr));
ValuePtr Handle::stdin(new Handle(&std::cin));
ValuePtr Handle::stdout(new Handle(&std::cout));

Handle::Handle(const type value) : value(value) {}

MutableValuePtr
Handle::copy() const {
  return std::make_shared<Handle>(*this);
}

void
Handle::write(std::ostream& stream) const {
  stream << "<Handle " << value << ">";
}
