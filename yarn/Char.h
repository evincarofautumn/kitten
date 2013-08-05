#ifndef CHAR_H_4721FD2A_CFC3_4741_B2EC_79A07ACD7EC0
#define CHAR_H_4721FD2A_CFC3_4741_B2EC_79A07ACD7EC0

#include "Value.h"

struct Char : Value {

  typedef uint32_t type;

  explicit Char(type);
  virtual MutableValuePtr copy() const final override;
  virtual void write(std::ostream& stream) const final override;

  type value;

};

#endif
