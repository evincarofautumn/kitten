#ifndef OPTION_H_3554BF12_377A_4CB7_BF2B_372ED6CA75BC
#define OPTION_H_3554BF12_377A_4CB7_BF2B_372ED6CA75BC

#include "Value.h"

struct Option : Value {

  typedef ValuePtr type;

  Option() = default;
  explicit Option(type);
  virtual MutableValuePtr copy() const final override;
  virtual void write(std::ostream& stream) const final override;

  type value;

};

#endif
