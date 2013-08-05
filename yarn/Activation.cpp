#include "Activation.h"

#include "ClosedName.h"

#include <ostream>

Activation::Activation(
  const LabelName label,
  const State& state,
  const std::vector<ClosedName>& names)
  : label(label) {
  for (const auto& name : names)
    closure.push_back(get_closed_name(state, name));
}

MutableValuePtr
Activation::copy() const {
  return std::make_shared<Activation>(*this);
}

void
Activation::write(std::ostream& stream) const {
  stream << "<activation " << label;
  for (const auto& value : closure)
    stream << " " << *value;
  stream << ">";
}
