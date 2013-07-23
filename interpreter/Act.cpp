#include "Act.h"

#include "Activation.h"
#include "State.h"

#include <ostream>

Offset
Act::exec(State& state) const {
  state.push_data(std::make_shared<Activation>(label, state, names));
  return 1;
}

void
Act::write(std::ostream& stream) const {
  stream << "act " << label << ' ';
  for (const auto name : names)
    stream
      << int(name.segment)
      << ':'
      << name.offset
      << ' ';
}
