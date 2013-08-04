#include "JumpIfRight.h"

#include "Choice.h"
#include "State.h"

#include <ostream>

void
JumpIfRight::write(std::ostream& stream) const {
  stream << "jn " << offset;
}

Offset
JumpIfRight::exec(State& state) const {
  const auto a = state.pop_data();
  const auto& choice = a->as<Choice>();
  state.push_data(choice.value);
  return (choice.is_right ? offset : 0) + 1;
}
