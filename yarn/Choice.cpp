#include "Choice.h"

#include <ostream>

Choice::Choice(const bool is_right, const std::shared_ptr<const Value> value)
  : is_right(is_right), value(value) {}

std::shared_ptr<Value>
Choice::copy() const {
  return std::make_shared<Choice>(*this);
}

void
Choice::write(std::ostream& stream) const {
  stream << (is_right ? "right" : "left") << ' ' << *value;
}
