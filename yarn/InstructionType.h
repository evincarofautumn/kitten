#ifndef INSTRUCTIONTYPE_H_E3EEBB40_D7E7_4551_BFA6_F4E9D0FF1120
#define INSTRUCTIONTYPE_H_E3EEBB40_D7E7_4551_BFA6_F4E9D0FF1120

#include <string>
#include <unordered_map>

enum class InstructionType {
  ACT,
  BUILTIN,
  CALL,
  CLOSURE,
  ENTER,
  ENTRY,
  JUMP,
  JUMP_IF_FALSE,
  JUMP_IF_NONE,
  JUMP_IF_RIGHT,
  LEAVE,
  LABEL,
  LOCAL,
  MAKE_VECTOR,
  PUSH,
  RETURN,
};

extern const std::unordered_map<std::string, InstructionType> INSTRUCTION_TYPE;

#endif
