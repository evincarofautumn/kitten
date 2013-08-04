#include "Label.h"

#include <ostream>

Offset
Label::exec(State&) const {
  return 1;
}

void
Label::write(std::ostream& stream) const {
  stream << "label " << name;
}
