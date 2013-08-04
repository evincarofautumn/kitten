#ifndef LOCAL_H_C5548BAC_C51C_433D_A36C_7DB095B6943E
#define LOCAL_H_C5548BAC_C51C_433D_A36C_7DB095B6943E

#include "Instruction.h"

struct Local : Instruction {

  virtual Offset exec(State&) const final override;
  virtual void write(std::ostream& stream) const final override;

  LocalName name;

};

#endif
