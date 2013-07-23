#ifndef CHOICE_H_3C000990_5298_434E_97F3_4C0E75B81E95
#define CHOICE_H_3C000990_5298_434E_97F3_4C0E75B81E95

#include "Value.h"

struct Choice : Value {

  Choice(bool, std::shared_ptr<const Value>);
  virtual std::shared_ptr<Value> copy() const final override;
  virtual void write(std::ostream&) const final override;

  bool is_right;
  std::shared_ptr<const Value> value;

};

#endif
