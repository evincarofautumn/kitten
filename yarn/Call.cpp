#include "Call.h"

#include "State.h"

#include <ostream>

Offset
Call::exec(State& state) const {
  state.call_label(label);
  return 0;
}

void
Call::write(std::ostream& stream) const {
  stream << "call " << label;
}
