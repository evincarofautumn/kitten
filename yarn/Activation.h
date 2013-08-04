#ifndef ACTIVATION_H_066DA9F7_AB6C_447D_B88F_C364958E1951
#define ACTIVATION_H_066DA9F7_AB6C_447D_B88F_C364958E1951

#include "Value.h"

#include "State.h"
#include "Types.h"

struct Activation : Value {

  Activation(LabelName, const State&, const std::vector<ClosedName>&);

  virtual std::shared_ptr<Value> copy() const final override;
  virtual void write(std::ostream& stream) const final override;

  std::vector<std::shared_ptr<const Value>> closure;
  LabelName label;

};

#endif
