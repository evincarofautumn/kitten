#ifndef INSTRUCTION_H_0768DB5D_6E9E_415F_9CAC_B8855EEFDD2E
#define INSTRUCTION_H_0768DB5D_6E9E_415F_9CAC_B8855EEFDD2E

#include "Types.h"

#include <iosfwd>

class State;

struct Instruction {

  Instruction() = default;
  virtual ~Instruction();

  friend std::ostream& operator<<(std::ostream&, const Instruction&);
  Offset execute(State&) const;

  template<class T>
  T& as() {
    return dynamic_cast<T&>(*this);
  }

  template<class T>
  const T& as() const {
    return dynamic_cast<const T&>(*this);
  }

protected:

  virtual Offset exec(State&) const = 0;
  virtual void write(std::ostream&) const = 0;

};

#endif
