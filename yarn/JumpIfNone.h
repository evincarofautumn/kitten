#ifndef JUMPIFNONE_H_8ACF5CF5_7E2C_4D39_95DA_3777B03DB164
#define JUMPIFNONE_H_8ACF5CF5_7E2C_4D39_95DA_3777B03DB164

#include "Instruction.h"

struct JumpIfNone : Instruction {

  virtual Offset exec(State&) const final override;
  virtual void write(std::ostream& stream) const final override;

  Offset offset;

};

#endif
