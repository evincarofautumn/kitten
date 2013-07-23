#ifndef FLOAT_H_1E171763_3B1E_476E_8EA9_4332C754E86D
#define FLOAT_H_1E171763_3B1E_476E_8EA9_4332C754E86D

#include "Value.h"

struct Float : Value {

  typedef double type;
  explicit Float(type);

  virtual std::shared_ptr<Value> copy() const final override;
  virtual void write(std::ostream&) const final override;

  type value;

};

#endif
