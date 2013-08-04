#ifndef MAKEVECTOR_H_0E960058_34B4_4FE8_955B_E36A9C09E133
#define MAKEVECTOR_H_0E960058_34B4_4FE8_955B_E36A9C09E133

#include "Instruction.h"

struct MakeVector : Instruction {

  virtual Offset exec(State&) const final override;
  virtual void write(std::ostream& stream) const final override;

  size_t size;

};

#endif
