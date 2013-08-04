#ifndef ENTER_H_EC0BCB3E_DB4E_498E_B09D_A8DFF6672C03
#define ENTER_H_EC0BCB3E_DB4E_498E_B09D_A8DFF6672C03

#include "Instruction.h"

struct Enter : Instruction {

  virtual Offset exec(State&) const final override;
  virtual void write(std::ostream& stream) const final override;

};

#endif
