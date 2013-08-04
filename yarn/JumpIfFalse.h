#ifndef JUMPIFFALSE_H_CB932762_75AE_45EB_88DB_A9F5CDF17EC6
#define JUMPIFFALSE_H_CB932762_75AE_45EB_88DB_A9F5CDF17EC6

#include "Instruction.h"

struct JumpIfFalse : Instruction {

  virtual Offset exec(State&) const final override;
  virtual void write(std::ostream& stream) const final override;

  Offset offset;

};

#endif
