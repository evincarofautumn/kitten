#include "Choice.h"

#include <ostream>

Choice::Choice(const bool is_right, const type value)
  : is_right(is_right), value(value) {}

MutableValuePtr
Choice::copy() const {
  return std::make_shared<Choice>(*this);
}

void
Choice::write(std::ostream& stream) const {
  stream << (is_right ? "right" : "left") << ' ' << *value;
}
