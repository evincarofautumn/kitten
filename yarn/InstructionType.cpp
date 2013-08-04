#include "InstructionType.h"

const std::unordered_map<std::string, InstructionType>
INSTRUCTION_TYPE {
  std::make_pair("act", InstructionType::ACT),
  std::make_pair("builtin", InstructionType::BUILTIN),
  std::make_pair("call", InstructionType::CALL),
  std::make_pair("closure", InstructionType::CLOSURE),
  std::make_pair("enter", InstructionType::ENTER),
  std::make_pair("entry", InstructionType::ENTRY),
  std::make_pair("jmp", InstructionType::JUMP),
  std::make_pair("jf", InstructionType::JUMP_IF_FALSE),
  std::make_pair("jn", InstructionType::JUMP_IF_NONE),
  std::make_pair("jr", InstructionType::JUMP_IF_RIGHT),
  std::make_pair("leave", InstructionType::LEAVE),
  std::make_pair("label", InstructionType::LABEL),
  std::make_pair("local", InstructionType::LOCAL),
  std::make_pair("vector", InstructionType::MAKE_VECTOR),
  std::make_pair("push", InstructionType::PUSH),
  std::make_pair("ret", InstructionType::RETURN),
};
