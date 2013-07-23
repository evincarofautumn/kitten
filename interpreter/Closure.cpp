#include "Closure.h"

#include "State.h"

#include <ostream>

Offset
Closure::exec(State& state) const {
  state.push_data(state.get_closure(name));
  return 1;
}

void
Closure::write(std::ostream& stream) const {
  stream << "closure " << name;
}
