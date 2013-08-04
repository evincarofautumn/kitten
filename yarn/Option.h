#ifndef OPTION_H_3554BF12_377A_4CB7_BF2B_372ED6CA75BC
#define OPTION_H_3554BF12_377A_4CB7_BF2B_372ED6CA75BC

#include "Value.h"

struct Option : Value {

  Option() = default;
  explicit Option(std::shared_ptr<const Value>);
  virtual std::shared_ptr<Value> copy() const final override;
  virtual void write(std::ostream& stream) const final override;

  std::shared_ptr<const Value> value;

};

#endif
