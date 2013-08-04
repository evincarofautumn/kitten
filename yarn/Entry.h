#ifndef ENTRY_H_823BB753_A004_4F92_AC16_63E1DE845E9B
#define ENTRY_H_823BB753_A004_4F92_AC16_63E1DE845E9B

#include "Instruction.h"

struct Entry : Instruction {

  virtual Offset
  exec(State&) const final override {
    return 1;
  }

  virtual void write(std::ostream& stream) const final override;

};

#endif
