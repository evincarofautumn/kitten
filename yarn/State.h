#ifndef STATE_H_3147C6FE_D646_4F3D_9C8F_3602CDC1BBAC
#define STATE_H_3147C6FE_D646_4F3D_9C8F_3602CDC1BBAC

#include "Instruction.h"
#include "Types.h"

#include <map>
#include <memory>
#include <vector>

#include "Value.h"

class State {

  enum class CallType {
    FUNCTION,
    CLOSURE,
  };

  struct CallEntry {
    Address return_address;
    CallType call_type;
  };

  const std::map<LabelName, Address>& label_offset;
  const std::vector<std::shared_ptr<Instruction>>& instructions;
  Address ip;
  std::vector<std::vector<ValuePtr>> closure;
  std::vector<ValuePtr> data;
  std::vector<ValuePtr> local;

  std::vector<CallEntry> call_stack;
  bool running;

public:

  State(
    Address,
    const std::map<LabelName, Address>&,
    const std::vector<std::shared_ptr<Instruction>>&);

  friend std::ostream& operator<<(std::ostream&, const State&);

  ValuePtr get_closure(size_t) const;
  ValuePtr get_local(size_t) const;
  ValuePtr pop_data();

  void advance();

  void call_label(LabelName); 
  void call_with_closure(
    const std::vector<ValuePtr>&,
    LabelName);

  void drop_data();
  void drop_local();

  void exit() {
    running = false;
  }

  bool is_running() const {
    return running;
  }

  void push_data(ValuePtr);
  void push_local(ValuePtr);

  void ret();

};

class ClosedName;

ValuePtr
get_closed_name(const State&, const ClosedName&);

#endif
