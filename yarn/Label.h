#ifndef LABEL_H_F4868E16_6BAF_41AC_88F8_7D45BB594A58
#define LABEL_H_F4868E16_6BAF_41AC_88F8_7D45BB594A58

#include "Instruction.h"

struct Label : Instruction {

  virtual Offset exec(State&) const final override;
  virtual void write(std::ostream& stream) const final override;
  LabelName name;

};

#endif
