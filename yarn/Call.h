#ifndef CALL_H_D511AEA5_FC9D_43A4_A4DB_842A6EA53105
#define CALL_H_D511AEA5_FC9D_43A4_A4DB_842A6EA53105

#include "Instruction.h"

struct Call : Instruction {

  virtual Offset exec(State&) const final override;
  virtual void write(std::ostream& stream) const final override;

  LabelName label;

};

#endif
