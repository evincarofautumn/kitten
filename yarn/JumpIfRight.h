#ifndef JUMPIFRIGHT_H_71AA341C_CE15_4961_9C4F_5003E795751F
#define JUMPIFRIGHT_H_71AA341C_CE15_4961_9C4F_5003E795751F

#include "Instruction.h"

struct JumpIfRight : Instruction {

  virtual Offset exec(State&) const final override;
  virtual void write(std::ostream& stream) const final override;

  Offset offset;

};

#endif
