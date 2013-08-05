#include "Pair.h"

#include <ostream>

Pair::Pair(
  const ValuePtr first,
  const ValuePtr second)
  : first(first),
    second(second) {}

MutableValuePtr
Pair::copy() const {
  return std::make_shared<Pair>(*this);
}

void
Pair::write(std::ostream& stream) const {
  stream << *first << " & " << *second;
}
