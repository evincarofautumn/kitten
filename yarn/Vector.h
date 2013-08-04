#ifndef VECTOR_H_83875782_0BF0_4DA3_AD62_4B5D2A64EBAB
#define VECTOR_H_83875782_0BF0_4DA3_AD62_4B5D2A64EBAB

#include "Value.h"

#include <vector>

struct Vector : Value {

  typedef std::vector<std::shared_ptr<const Value>> type;

  Vector() = default;

  explicit Vector(const type&);
  virtual std::shared_ptr<Value> copy() const final override;
  virtual void write(std::ostream&) const final override;

  type value;

};

#endif
