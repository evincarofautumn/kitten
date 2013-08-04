#include "Enter.h"

#include "State.h"

#include <ostream>

Offset
Enter::exec(State& state) const {
  state.push_local(state.pop_data());
  return 1;
}

void
Enter::write(std::ostream& stream) const {
  stream << "enter";
}
