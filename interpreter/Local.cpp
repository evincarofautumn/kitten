#include "Local.h"

#include "State.h"

#include <ostream>

Offset
Local::exec(State& state) const {
  state.push_data(state.get_local(name));
  return 1;
}

void
Local::write(std::ostream& stream) const {
  stream << "local " << name;
}
