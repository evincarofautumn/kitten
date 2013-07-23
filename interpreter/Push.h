#ifndef PUSH_H_1582B0E7_F243_4F11_B731_0CDD4C71E029
#define PUSH_H_1582B0E7_F243_4F11_B731_0CDD4C71E029

#include "Instruction.h"

#include "Value.h"

struct Push : Instruction {

  virtual Offset exec(State&) const final override;
  virtual void write(std::ostream& stream) const final override;

  std::shared_ptr<const Value> value;

};

#endif
