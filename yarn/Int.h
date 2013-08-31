#ifndef INT_H_D2BB0E1E_66A8_4ED8_A70E_EEF2FACAA39A
#define INT_H_D2BB0E1E_66A8_4ED8_A70E_EEF2FACAA39A

#include "Value.h"

struct Int : Value {

  typedef int64_t type;

  explicit Int(type);
  virtual MutableValuePtr copy() const final override;
  virtual void write(std::ostream& stream) const final override;

  type value;

};

#endif
