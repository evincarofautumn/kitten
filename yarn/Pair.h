#ifndef PAIR_H_1539388A_2D66_451D_8808_E438A04F2DD8
#define PAIR_H_1539388A_2D66_451D_8808_E438A04F2DD8

#include "Value.h"

struct Pair : Value {

  Pair(ValuePtr, ValuePtr);
  virtual MutableValuePtr copy() const final override;
  virtual void write(std::ostream& stream) const;

  ValuePtr first;
  ValuePtr second;

};

#endif
