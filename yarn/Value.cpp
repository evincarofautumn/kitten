#include "Value.h"

std::ostream& operator<<(std::ostream& stream, const Value& value) {
  value.write(stream);
  return stream;
}
