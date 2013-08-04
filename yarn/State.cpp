#include "State.h"

#include "ClosedName.h"

#include <ostream>
#include <sstream>

State::State(
  const Address ip,
  const std::map<LabelName, Address>& label_offset,
  const std::vector<std::shared_ptr<Instruction>>& instructions)
  : label_offset(label_offset),
    instructions(instructions),
    ip(ip),
    running(true) {}

std::ostream& operator<<(std::ostream& stream, const State& state) {
  stream
    << "*** State ***\nData ("
    << state.data.size()
    << "):\n";
  for (const auto& value : state.data)
    stream << "  " << *value << "\n";
  stream
    << "\nLocal ("
    << state.local.size()
    << "):\n";
  for (const auto& value : state.local)
    stream << "  " << *value << "\n";
  if (!state.closure.empty()) {
    stream
      << "\nClosure ("
      << state.closure.back().size()
      << "):\n";
    for (const auto& value : state.closure.back())
      stream << "  " << *value << "\n";
  }
  stream
    << "\nCall ("
    << state.call_stack.size()
    << "):\n";
  for (const auto& entry : state.call_stack)
    stream
      << "  " << int(entry.call_type)
      << "  " << entry.return_address << "\n";
  return stream << "\nIP: " << state.ip << "\n";
}

void
State::advance() {
  ip += instructions.at(ip)->execute(*this);
}

void
State::call_label(const LabelName label) {
  call_stack.push_back(CallEntry{ip + 1, CallType::FUNCTION});
  ip = label_offset.at(label);
}

void
State::call_with_closure(
  const std::vector<std::shared_ptr<const Value>>& values,
  const LabelName label) {
  closure.push_back(values);
  call_stack.push_back(CallEntry{ip + 1, CallType::CLOSURE});
  ip = label_offset.at(label);
}

void
State::drop_data() {
  if (local.empty())
    throw std::runtime_error("Stack underflow in drop_data()");
  data.pop_back();
}

void
State::drop_local() {
  if (local.empty())
    throw std::runtime_error("Stack underflow in drop_local()");
  local.pop_back();
}

std::shared_ptr<const Value>
State::get_local(const size_t offset) const {
  return *(local.rbegin() + offset);
}

std::shared_ptr<const Value>
State::get_closure(const size_t offset) const try {
  return closure.back().at(offset);
} catch (const std::exception&) {
  std::ostringstream message;
  message << "In get_closure(" << offset << ")";
  throw_with_nested(std::runtime_error(message.str()));
}

void
State::push_data(const std::shared_ptr<const Value> value) {
  data.push_back(value);
}

void
State::push_local(const std::shared_ptr<const Value> value) {
  local.push_back(value);
}

std::shared_ptr<const Value>
State::pop_data() {
  if (data.empty())
    throw std::runtime_error("Stack underflow in pop_data()");
  auto value = data.back();
  data.pop_back();
  return value;
}

void
State::ret() {

  if (call_stack.empty()) {
    running = false;
    return;
  }

  if (call_stack.back().call_type == CallType::CLOSURE) {
    if (closure.empty())
      throw std::runtime_error("Closure stack underflow in return");
    closure.pop_back();
  }

  ip = call_stack.back().return_address;
  call_stack.pop_back();

}

std::shared_ptr<const Value>
get_closed_name(const State& state, const ClosedName& name) {
  switch (name.segment) {
  case Segment::INVALID:
    throw std::runtime_error("Invalid segment");
  case Segment::LOCAL:
    return state.get_local(name.offset);
  case Segment::CLOSED:
    return state.get_closure(name.offset);
  }
}
