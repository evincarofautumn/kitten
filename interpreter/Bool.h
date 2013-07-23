#ifndef BOOL_H_46B023F3_BF26_406D_92C9_36CA1455A5E6
#define BOOL_H_46B023F3_BF26_406D_92C9_36CA1455A5E6

#include "Value.h"

struct Bool : Value {

  typedef bool type;
  explicit Bool(type);

  virtual std::shared_ptr<Value> copy() const final override;
  virtual void write(std::ostream& stream) const final override;

  type value;

};

#endif
