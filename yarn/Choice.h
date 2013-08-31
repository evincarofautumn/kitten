#ifndef CHOICE_H_3C000990_5298_434E_97F3_4C0E75B81E95
#define CHOICE_H_3C000990_5298_434E_97F3_4C0E75B81E95

#include "Value.h"

struct Choice : Value {

  typedef ValuePtr type;

  enum class Which {
    LEFT,
    RIGHT,
  };

  Choice(Which, type);
  virtual MutableValuePtr copy() const final override;
  virtual void write(std::ostream&) const final override;

  static std::shared_ptr<Choice>
  make_left(const type value) {
    return std::make_shared<Choice>(Which::LEFT, value);
  }

  static std::shared_ptr<Choice>
  make_right(const type value) {
    return std::make_shared<Choice>(Which::RIGHT, value);
  }

  Which which;
  type value;

};

#endif
