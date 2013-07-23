#include "Pair.h"

#include <ostream>

Pair::Pair(
  const std::shared_ptr<const Value> first,
  const std::shared_ptr<const Value> second)
  : first(first),
    second(second) {}

std::shared_ptr<Value>
Pair::copy() const {
  return std::make_shared<Pair>(*this);
}

void
Pair::write(std::ostream& stream) const {
  stream << *first << " & " << *second;
}
