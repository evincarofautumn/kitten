#ifndef LEAVE_H_3F83F03E_B70C_48A0_BD01_DCD54F10594D
#define LEAVE_H_3F83F03E_B70C_48A0_BD01_DCD54F10594D

#include "Instruction.h"

struct Leave : Instruction {

  virtual Offset exec(State&) const final override;
  virtual void write(std::ostream& stream) const final override;

};

#endif
