#ifndef JUMP_H_A849CC8F_DBB5_4E6D_9E15_4820B3C324F2
#define JUMP_H_A849CC8F_DBB5_4E6D_9E15_4820B3C324F2

#include "Instruction.h"

struct Jump : Instruction {

  virtual Offset exec(State&) const final override;
  virtual void write(std::ostream& stream) const final override;

  Offset offset;

};

#endif
