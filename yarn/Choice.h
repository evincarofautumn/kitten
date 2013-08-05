#ifndef CHOICE_H_3C000990_5298_434E_97F3_4C0E75B81E95
#define CHOICE_H_3C000990_5298_434E_97F3_4C0E75B81E95

#include "Value.h"

struct Choice : Value {

  typedef ValuePtr type;

  Choice(bool, type);
  virtual MutableValuePtr copy() const final override;
  virtual void write(std::ostream&) const final override;

  bool is_right;
  type value;

};

#endif
