#include "MakeVector.h"

#include "State.h"
#include "Vector.h"

#include <ostream>

void
MakeVector::write(std::ostream& stream) const {
  stream << "make_vector " << size;
}

Offset
MakeVector::exec(State& state) const {
  auto vector = std::make_shared<Vector>();
  for (size_t i = 0; i < size; ++i)
    vector->value.push_back(state.pop_data());
  reverse(begin(vector->value), end(vector->value));
  state.push_data(vector);
  return 1;
}
