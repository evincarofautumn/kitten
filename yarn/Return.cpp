#include "Return.h"

#include "State.h"

#include <ostream>

void
Return::write(std::ostream& stream) const {
  stream << "ret";
}

Offset
Return::exec(State& state) const {
  state.ret();
  return 0;
}
