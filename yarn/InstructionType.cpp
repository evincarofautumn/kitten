#include "InstructionType.h"

const std::unordered_map<std::string, InstructionType>
INSTRUCTION_TYPE {
  {"act"     , InstructionType::ACT},
  {"builtin" , InstructionType::BUILTIN},
  {"call"    , InstructionType::CALL},
  {"closure" , InstructionType::CLOSURE},
  {"enter"   , InstructionType::ENTER},
  {"entry"   , InstructionType::ENTRY},
  {"jf"      , InstructionType::JUMP_IF_FALSE},
  {"jmp"     , InstructionType::JUMP},
  {"jn"      , InstructionType::JUMP_IF_NONE},
  {"jr"      , InstructionType::JUMP_IF_RIGHT},
  {"label"   , InstructionType::LABEL},
  {"leave"   , InstructionType::LEAVE},
  {"local"   , InstructionType::LOCAL},
  {"push"    , InstructionType::PUSH},
  {"ret"     , InstructionType::RETURN},
  {"vector"  , InstructionType::MAKE_VECTOR},
};
