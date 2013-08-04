#include "Instruction.h"

#include <sstream>
#include <stdexcept>

Instruction::~Instruction() {}

std::ostream& operator<<(std::ostream& stream, const Instruction& instruction) {
  instruction.write(stream);
  return stream;
}

Offset Instruction::execute(State& state) const try {
  return exec(state);
} catch (const std::exception&) {
  std::ostringstream message;
  message << "Executing " << *this;
  throw_with_nested(std::runtime_error(message.str()));
}
