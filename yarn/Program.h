#ifndef PROGRAM_H_FFFED766_CE32_42EE_B68A_29DFC52B408D
#define PROGRAM_H_FFFED766_CE32_42EE_B68A_29DFC52B408D

#include "Instruction.h"
#include "Value.h"

#include <map>
#include <vector>

class ClosedName;

class Program {
public:

  Program(std::istream& stream) {
    read_program(stream);
  }

  void
  run() const;

private:

  void
  read_program(std::istream&);

  std::shared_ptr<Instruction>
  read_instruction(std::istream&);

  ValuePtr
  read_value(std::istream&);

  bool
  read_closed_name(std::istream&, ClosedName&);

  std::map<LabelName, Address> label_offset;
  std::vector<std::shared_ptr<Instruction>> instructions;

};

#endif
