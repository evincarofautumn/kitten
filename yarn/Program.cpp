#include "Program.h"

#include "Act.h"
#include "Bool.h"
#include "Builtin.h"
#include "BuiltinId.h"
#include "Call.h"
#include "Char.h"
#include "ClosedName.h"
#include "Closure.h"
#include "Enter.h"
#include "Entry.h"
#include "Float.h"
#include "InstructionType.h"
#include "Int.h"
#include "Jump.h"
#include "JumpIfFalse.h"
#include "JumpIfNone.h"
#include "JumpIfRight.h"
#include "Label.h"
#include "Leave.h"
#include "Local.h"
#include "MakeVector.h"
#include "Push.h"
#include "Return.h"
#include "State.h"
#include "Unit.h"
#include "ValueType.h"
#include "Vector.h"

#include <iostream>
#include <sstream>
#include <string>

const LabelName ENTRY_LABEL = LabelName(-1);

void
Program::read_program(std::istream& stream) {

  std::string line;
  std::shared_ptr<Instruction> instruction;

  while (std::getline(stream, line)) {

    if (line.empty() or line.at(0) == ';')
      continue;

    std::istringstream line_stream(move(line));

    if (!(instruction = read_instruction(line_stream))) {
      std::ostringstream message;
      message
        << "Unable to read instruction: "
        << line_stream.str();
      throw std::runtime_error(message.str());
    }

    if (const auto label
      = std::dynamic_pointer_cast<Label>(instruction)) {
      label_offset[label->name] = instructions.size();
    } else if (const auto entry
      = std::dynamic_pointer_cast<Entry>(instruction)) {
      label_offset[ENTRY_LABEL] = instructions.size();
    }

    instructions.push_back(instruction);

  }

}

std::shared_ptr<Instruction>
Program::read_instruction(std::istream& stream) {

  std::string mnemonic;
  if (!(stream >> mnemonic))
    return nullptr;
  
  const auto instruction_type
    = INSTRUCTION_TYPE.find(mnemonic);

  if (instruction_type == end(INSTRUCTION_TYPE))
    return nullptr;

  switch (instruction_type->second) {

  case InstructionType::ACT:
  {
    auto act = std::make_shared<Act>();
    if (!(stream >> act->label))
      return nullptr;
    {
      ClosedName name;
      while (read_closed_name(stream, name))
        act->names.push_back(name);
    }
    return act;
  }

  case InstructionType::BUILTIN:
  {
    auto builtin = std::make_shared<Builtin>();
    std::string mnemonic;
    if (!(stream >> mnemonic))
      return nullptr;
    
    auto id = BUILTIN.find(mnemonic);
    if (id == end(BUILTIN))
      return nullptr;
    
    builtin->id = id->second;
    return builtin;
  }

  case InstructionType::CALL:
  {
    auto call = std::make_shared<Call>();
    if (!(stream >> call->label))
      return nullptr;
    return call;
  }

  case InstructionType::CLOSURE:
  {
    auto closure = std::make_shared<Closure>();
    if (!(stream >> closure->name))
      return nullptr;
    return closure;
  }

  case InstructionType::ENTER:
    return std::make_shared<Enter>();

  case InstructionType::ENTRY:
    return std::make_shared<Entry>();

  case InstructionType::JUMP:
  {
    auto jump = std::make_shared<Jump>();
    if (!(stream >> jump->offset))
      return nullptr;
    return jump;
  }

  case InstructionType::JUMP_IF_FALSE:
  {
    auto jump_if_false = std::make_shared<JumpIfFalse>();
    if (!(stream >> jump_if_false->offset))
      return nullptr;
    return jump_if_false;
  }

  case InstructionType::JUMP_IF_NONE:
  {
    auto jump_if_none = std::make_shared<JumpIfNone>();
    if (!(stream >> jump_if_none->offset))
      return nullptr;
    return jump_if_none;
  }

  case InstructionType::JUMP_IF_RIGHT:
  {
    auto jump_if_right = std::make_shared<JumpIfRight>();
    if (!(stream >> jump_if_right->offset))
      return nullptr;
    return jump_if_right;
  }

  case InstructionType::LEAVE:
    return std::make_shared<Leave>();

  case InstructionType::LABEL:
  {
    auto label = std::make_shared<Label>();
    if (!(stream >> label->name))
      return nullptr;
    return label;
  }

  case InstructionType::LOCAL:
  {
    auto local = std::make_shared<Local>();
    if (!(stream >> local->name))
      return nullptr;
    return local;
  }

  case InstructionType::MAKE_VECTOR:
  {
    auto make_vector = std::make_shared<MakeVector>();
    if (!(stream >> make_vector->size))
      return nullptr;
    return make_vector;
  }

  case InstructionType::PUSH:
  {
    auto push = std::make_shared<Push>();
    if (!(push->value = read_value(stream)))
      return nullptr;
    return push;
  }

  case InstructionType::RETURN:
    return std::make_shared<Return>();

  }

}

ValuePtr
Program::read_value(std::istream& stream) {

  std::string mnemonic;
  if (!(stream >> mnemonic))
    return nullptr;
  
  const auto value_type(VALUE_TYPE.find(mnemonic));

  if (value_type == end(VALUE_TYPE))
    return nullptr;

  switch (value_type->second) {

  case ValueType::BOOL:
  {
    int as_bool;
    if (!(stream >> as_bool))
      return nullptr;
    return std::make_shared<Bool>(as_bool);
  }

  case ValueType::CHAR:
  {
    Char::type as_char;
    if (!(stream >> as_char))
      return nullptr;
    return std::make_shared<Char>(as_char);
  }

  case ValueType::FLOAT:
  {
    Float::type as_float;
    if (!(stream >> as_float))
      return nullptr;
    return std::make_shared<Float>(as_float);
  }

  case ValueType::HANDLE:
    return nullptr;

  case ValueType::INT:
  {
    Int::type as_int;
    if (!(stream >> as_int))
      return nullptr;
    return std::make_shared<Int>(as_int);
  }

  case ValueType::VECTOR:
  {
    size_t length;
    if (!(stream >> length))
      return nullptr;

    auto vector = std::make_shared<Vector>();

    for (decltype(length) i = 0; i < length; ++i) {
      ValuePtr element;
      if (!(element = read_value(stream)))
        return nullptr;
      vector->value.push_back(element);
    }

    return vector;
  }

  case ValueType::PAIR:
    return nullptr;

  case ValueType::UNIT:
    return std::make_shared<Unit>();

  }

}

bool
Program::read_closed_name(std::istream& stream, ClosedName& result) {

  std::string token;
  if (!(stream >> token))
    return false;

  std::istringstream token_stream(token);

  std::string segment;
  if (!getline(token_stream, segment, ':'))
    return false;

  uint16_t index;
  if (!(token_stream >> index))
    return false;

  if (segment == "local") {
    result = ClosedName(Segment::LOCAL, index);
    return true;
  }

  if (segment == "closure") {
    result = ClosedName(Segment::CLOSED, index);
    return true;
  }

  throw std::runtime_error("Invalid activation instruction");

}

void
Program::run() const {

  const auto ip_label = label_offset.find(ENTRY_LABEL);
  if (ip_label == end(label_offset)) {
    std::cerr << "No entry point.";
    return;
  }

  State state(ip_label->second, label_offset, instructions);
  try {
    while (state.is_running())
      state.advance();
  } catch (const std::runtime_error& e) {
    std::ostringstream message;
    message << state;
    throw_with_nested(std::runtime_error(message.str()));
  }
}
