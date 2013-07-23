#ifndef PAIR_H_1539388A_2D66_451D_8808_E438A04F2DD8
#define PAIR_H_1539388A_2D66_451D_8808_E438A04F2DD8

#include "Value.h"

struct Pair : Value {

  Pair(std::shared_ptr<const Value>, std::shared_ptr<const Value>);
  virtual std::shared_ptr<Value> copy() const final override;
  virtual void write(std::ostream& stream) const;

  std::shared_ptr<const Value> first;
  std::shared_ptr<const Value> second;

};

#endif
