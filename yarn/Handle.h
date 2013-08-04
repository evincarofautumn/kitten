#ifndef HANDLE_H_A6CC41F8_7707_4DB6_A937_F7636A9EBE65
#define HANDLE_H_A6CC41F8_7707_4DB6_A937_F7636A9EBE65

#include "Value.h"

struct Handle : Value {

  explicit Handle(void*);
  virtual std::shared_ptr<Value> copy() const final override;
  virtual void write(std::ostream& stream) const final override;

  void* value;

};

#endif
