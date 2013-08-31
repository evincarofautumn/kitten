#include "Choice.h"

#include <ostream>

Choice::Choice(const Which which, const type value)
  : which(which), value(value) {}

MutableValuePtr
Choice::copy() const {
  return std::make_shared<Choice>(*this);
}

void
Choice::write(std::ostream& stream) const {
  stream << (which == Which::LEFT ? "left" : "right") << ' ' << *value;
}
