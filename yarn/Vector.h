#ifndef VECTOR_H_83875782_0BF0_4DA3_AD62_4B5D2A64EBAB
#define VECTOR_H_83875782_0BF0_4DA3_AD62_4B5D2A64EBAB

#include "Value.h"

#include <vector>

struct Vector : Value {

  typedef std::vector<ValuePtr> type;

  Vector() = default;

  explicit Vector(const type&);
  virtual MutableValuePtr copy() const final override;
  virtual void write(std::ostream&) const final override;

  type value;

};

#endif
