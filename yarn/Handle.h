#ifndef HANDLE_H_A6CC41F8_7707_4DB6_A937_F7636A9EBE65
#define HANDLE_H_A6CC41F8_7707_4DB6_A937_F7636A9EBE65

#include "Value.h"

struct Handle : Value {

  typedef void* type;

  explicit Handle(type);
  virtual MutableValuePtr copy() const final override;
  virtual void write(std::ostream& stream) const final override;

  type value;

};

#endif
