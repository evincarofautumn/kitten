#include "Push.h"

#include "State.h"

#include <ostream>

Offset
Push::exec(State& state) const {
  state.push_data(value);
  return 1;
}

void
Push::write(std::ostream& stream) const {
  stream << "push " << *value;
}
