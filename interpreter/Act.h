#ifndef ACT_H_9D3111E3_4630_4E74_A1AC_CCFBF873B811
#define ACT_H_9D3111E3_4630_4E74_A1AC_CCFBF873B811

#include "Instruction.h"

#include "ClosedName.h"

#include <vector>

struct Act : Instruction {

  virtual Offset exec(State&) const final override;
  virtual void write(std::ostream&) const final override;

  LabelName label;
  std::vector<ClosedName> names;

};

#endif
