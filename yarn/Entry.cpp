#include "Entry.h"

#include <ostream>

void
Entry::write(std::ostream& stream) const {
  stream << "entry";
}
