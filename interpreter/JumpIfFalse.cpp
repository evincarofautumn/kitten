#include "JumpIfFalse.h"

#include "Bool.h"
#include "State.h"

#include <ostream>

void
JumpIfFalse::write(std::ostream& stream) const {
  stream << "jf " << offset;
}

Offset
JumpIfFalse::exec(State& state) const {
  const auto a = state.pop_data();
  return (a->as<Bool>().value ? 0 : offset) + 1;
}
