#ifndef UNIT_H_398D8EC3_3A8D_45F3_ABAB_9F8829FB7764
#define UNIT_H_398D8EC3_3A8D_45F3_ABAB_9F8829FB7764

#include "Value.h"

struct Unit : Value {

  virtual std::shared_ptr<Value> copy() const final override;
  virtual void write(std::ostream&) const final override;

};

#endif
