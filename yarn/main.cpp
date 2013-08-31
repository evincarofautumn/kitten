#include "Instruction.h"
#include "Program.h"
#include "State.h"
#include "Types.h"
#include "Value.h"

#include <fstream>
#include <iostream>

void
print_exception(const std::exception& e, const unsigned depth = 0) {
  for (unsigned i = 0; i < depth; ++i)
    std::cerr << "  ";
  std::cerr << e.what() << '\n';
  try {
    std::rethrow_if_nested(e);
  } catch (const std::exception& e) {
    print_exception(e, depth + 1);
  } catch (...) {}
}

int main(int argc, char** argv) try {

  --argc;
  ++argv;

  switch (argc) {

  case 0:
  {
    Program program(std::cin);
    program.run();
    break;
  }

  case 1:
  {
    std::ifstream file(argv[0]);
    if (!file.is_open())
      throw std::runtime_error("bad input file");
    Program program(file);
    program.run();
    break;
  }

  default:
    throw std::runtime_error("usage: yarn [FILE]");

  }

} catch (const std::exception& e) {
  print_exception(e);
  return 1;
}
