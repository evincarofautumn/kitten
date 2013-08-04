#include "Leave.h"

#include "State.h"

#include <ostream>

Offset
Leave::exec(State& state) const {
  state.drop_local();
  return 1;
}

void
Leave::write(std::ostream& stream) const {
  stream << "leave";
}
