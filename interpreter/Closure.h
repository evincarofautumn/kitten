#ifndef CLOSURE_H_065C5AA4_778E_42BB_931A_32B129E7424B
#define CLOSURE_H_065C5AA4_778E_42BB_931A_32B129E7424B

#include "Instruction.h"

struct Closure : Instruction {

  virtual Offset exec(State&) const final override;
  virtual void write(std::ostream& stream) const final override;

  ClosureName name;

};

#endif
