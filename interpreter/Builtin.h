#ifndef BUILTIN_H_69094408_3BBA_4FF8_82CC_2440EC11DB3F
#define BUILTIN_H_69094408_3BBA_4FF8_82CC_2440EC11DB3F

#include "Instruction.h"

#include "BuiltinId.h"

struct Builtin : Instruction {

  virtual Offset exec(State&) const final override;
  virtual void write(std::ostream& stream) const final override;

  BuiltinId id;

};

#endif
