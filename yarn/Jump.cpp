#include "Jump.h"

#include "State.h"

#include <ostream>

Offset
Jump::exec(State& state) const {
  return offset + 1;
}

void
Jump::write(std::ostream& stream) const {
  stream << "jump " << offset;
}
