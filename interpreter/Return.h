#ifndef RETURN_H_25CDC007_F394_4D90_B423_07913E9E87D8
#define RETURN_H_25CDC007_F394_4D90_B423_07913E9E87D8

#include "Instruction.h"

struct Return : Instruction {

  virtual Offset exec(State&) const final override;
  virtual void write(std::ostream& stream) const final override;

};

#endif
