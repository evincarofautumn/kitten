#include "JumpIfNone.h"

#include "Option.h"
#include "State.h"

#include <ostream>

void
JumpIfNone::write(std::ostream& stream) const {
  stream << "jn " << offset;
}

Offset
JumpIfNone::exec(State& state) const {
  const auto a = state.pop_data();
  const auto& value = a->as<Option>().value;
  if (value) {
    state.push_data(value);
    return 1;
  }
  return offset + 1;
}
