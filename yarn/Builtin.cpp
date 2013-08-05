#include "Builtin.h"

#include "Activation.h"
#include "Bool.h"
#include "Char.h"
#include "Choice.h"
#include "Float.h"
#include "Handle.h"
#include "Int.h"
#include "Option.h"
#include "Pair.h"
#include "State.h"
#include "Vector.h"

#include <iostream>
#include <sstream>

template<class T, class F>
void
unary(State& state, F pure) {
  const auto a = state.pop_data();
  state.push_data(std::make_shared<T>(pure(a->as<T>().value)));
}

template<class T, class F>
void
binary(State& state, F pure) {
  const auto b = state.pop_data();
  const auto a = state.pop_data();
  state.push_data(
    std::make_shared<T>(
      pure(a->as<T>().value, b->as<T>().value)));
}

template<class T, class F>
void
relational(State& state, F function) {
  const auto b = state.pop_data();
  const auto a = state.pop_data();
  state.push_data(
    std::make_shared<Bool>(
      function(a->as<T>().value, b->as<T>().value)));
}

std::vector<ValuePtr>
add_vector(
  const std::vector<ValuePtr>& a,
  const std::vector<ValuePtr>& b) try {

  std::vector<ValuePtr> result;

  auto copy = [](const ValuePtr& value) {
    return value->copy();
  };

  transform(begin(a), end(a), back_inserter(result), copy);
  transform(begin(b), end(b), back_inserter(result), copy);
  return result;

} catch (const std::exception&) {
  throw_with_nested(std::runtime_error("In add_vector()"));
}

template<class T>
struct bit_not {
  T operator()(const T& value) {
    return ~value;
  }
};

ValuePtr
make_char_vector(const std::string& input) {
  const auto result = std::make_shared<Vector>();
  for (const auto character : input)
    result->value.push_back(std::make_shared<Char>(character));
  return result;
}

Offset
Builtin::exec(State& state) const {

  switch (id) {

    // case BuiltinId::AddFloat

  case BuiltinId::AddInt:
    binary<Int>(state, std::plus<Int::type>());
    break;

  case BuiltinId::AddVector:
    binary<Vector>(state, add_vector);
    break;

  case BuiltinId::AndBool:
    relational<Bool>(state, std::logical_and<Bool::type>());
    break;

  case BuiltinId::AndInt:
    binary<Int>(state, std::bit_and<Int::type>());
    break;

  case BuiltinId::Apply:
  {
    const auto activation = state.pop_data();
    state.call_with_closure(
      activation->as<Activation>().closure,
      activation->as<Activation>().label);
    return 0;
  }

  case BuiltinId::CharToInt:
  {
    const auto a = state.pop_data();
    state.push_data(
      std::make_shared<Int>(a->as<Char>().value));
    break;
  }

  // case BuiltinId::Close
  // case BuiltinId::DivFloat
  
  case BuiltinId::DivInt:
    binary<Int>(state, std::divides<Int::type>());
    break;

  // case BuiltinId::EqFloat

  case BuiltinId::EqInt:
    relational<Int>(state, std::equal_to<Int::type>());
    break;

  case BuiltinId::Exit:
    state.exit();
    break;

  case BuiltinId::First:
  {
    const auto a = state.pop_data();
    state.push_data(a->as<Pair>().first);
    break;
  }

  case BuiltinId::FromLeft:
  {
    const auto a = state.pop_data();
    state.push_data(a->as<Choice>().value);
    break;
  }

  // case BuiltinId::FromRight
  // case BuiltinId::FromSome
  // case BuiltinId::GeFloat

  case BuiltinId::GeInt:
  {
    relational<Int>(state, std::greater_equal<Int::type>());
    break;
  }

  case BuiltinId::Get:
  {
    const auto index = state.pop_data();
    const auto vector = state.pop_data();
    state.push_data(
      vector->as<Vector>().value.at(index->as<Int>().value));
    break;
  }

  case BuiltinId::GetLine:
  {
    const auto handle = state.pop_data();
    std::string line;
    std::getline(
      *static_cast<std::istream*>(handle->as<Handle>().value),
      line);
    state.push_data(make_char_vector(line));
    break;
  }

  // case BuiltinId::GtFloat

  case BuiltinId::GtInt:
  {
    relational<Int>(state, std::greater<Int::type>());
    break;
  }

  // case BuiltinId::Impure

  case BuiltinId::Init:
  {
    const auto result = state.pop_data()->copy();
    result->as<Vector>().value.pop_back();
    state.push_data(result);
    break;
  }

  // case BuiltinId::LeFloat

  case BuiltinId::LeInt:
  {
    relational<Int>(state, std::less_equal<Int::type>());
    break;
  }

  case BuiltinId::Left:
  {
    const auto a = state.pop_data();
    state.push_data(std::make_shared<Choice>(false, a));
    break;
  }

  case BuiltinId::Length:
  {
    const auto a = state.pop_data();
    state.push_data(
      std::make_shared<Int>(a->as<Vector>().value.size()));
    break;
  }

  // case BuiltinId::LtFloat

  case BuiltinId::LtInt:
  {
    relational<Int>(state, std::less<Int::type>());
    break;
  }

  // case BuiltinId::ModFloat

  case BuiltinId::ModInt:
  {
    binary<Int>(state, std::modulus<Int::type>());
    break;
  }

  // case BuiltinId::MulFloat
  
  case BuiltinId::MulInt:
  {
    binary<Int>(state, std::multiplies<Int::type>());
    break;
  }

  // case BuiltinId::NeFloat

  case BuiltinId::NeInt:
  {
    relational<Int>(state, std::not_equal_to<Int::type>());
    break;
  }

  // case BuiltinId::NegFloat

  case BuiltinId::NegInt:
  {
    unary<Int>(state, std::negate<Int::type>());
    break;
  }

  case BuiltinId::None:
  {
    state.push_data(std::make_shared<Option>());
    break;
  }

  case BuiltinId::NotBool:
  {
    const auto a = state.pop_data();
    state.push_data(std::make_shared<Bool>(!a->as<Bool>().value));
    break;
  }

  case BuiltinId::NotInt:
  {
    unary<Int>(state, bit_not<Int::type>());
    break;
  }

  // case BuiltinId::OpenIn
  // case BuiltinId::OpenOut

  case BuiltinId::OrBool:
  {
    relational<Bool>(state, std::logical_or<Bool::type>());
    break;
  }

  case BuiltinId::OrInt:
  {
    binary<Int>(state, std::bit_or<Int::type>());
    break;
  }

  case BuiltinId::Pair:
  {
    const auto b = state.pop_data();
    const auto a = state.pop_data();
    state.push_data(std::make_shared<Pair>(a, b));
    break;
  }

  case BuiltinId::Print:
  {
    const auto handle = state.pop_data();
    const auto text = state.pop_data();
    std::string buffer;
    for (const auto& value : text->as<Vector>().value) {
      // TODO Use correct character encoding.
      buffer.append(1, char(value->as<Char>().value));
    }
    *static_cast<std::ostream*>(handle->as<Handle>().value)
      << buffer;
    break;
  }

  case BuiltinId::Rest:
  {
    const auto a = state.pop_data();
    state.push_data(a->as<Pair>().second);
    break;
  }

  case BuiltinId::Right:
  {
    const auto a = state.pop_data();
    state.push_data(std::make_shared<Choice>(true, a));
    break;
  }

  case BuiltinId::Set:
  {
    const auto index = state.pop_data();
    const auto value = state.pop_data();
    const auto vector = state.pop_data()->copy();
    vector->as<Vector>().value.at(index->as<Int>().value) = value;
    state.push_data(vector);
    break;
  }

  case BuiltinId::ShowFloat:
  {
    const auto a = state.pop_data();
    std::ostringstream stream;
    stream << a->as<Float>().value;
    state.push_data(make_char_vector(stream.str()));
    break;
  }

  case BuiltinId::ShowInt:
  {
    const auto a = state.pop_data();
    std::ostringstream stream;
    stream << a->as<Int>().value;
    state.push_data(make_char_vector(stream.str()));
    break;
  }

  case BuiltinId::Some:
  {
    const auto a = state.pop_data();
    state.push_data(std::make_shared<Option>(a));
    break;
  }

  case BuiltinId::Stderr:
  {
    state.push_data(std::make_shared<Handle>(&std::cerr));
    break;
  }

  case BuiltinId::Stdin:
  {
    state.push_data(std::make_shared<Handle>(&std::cin));
    break;
  }

  case BuiltinId::Stdout:
  {
    state.push_data(std::make_shared<Handle>(&std::cout));
    break;
  }

  // case BuiltinId::SubFloat

  case BuiltinId::SubInt:
  {
    binary<Int>(state, std::minus<Int::type>());
    break;
  }

  case BuiltinId::Tail:
  {
    const auto result = state.pop_data()->copy();
    auto& vector = result->as<Vector>().value;
    vector.erase(begin(vector));
    state.push_data(result);
    break;
  }

  case BuiltinId::UnsafePurify11:
  {
    // Cast
    break;
  }

  // case BuiltinId::XorBool

  case BuiltinId::XorInt:
  {
    binary<Int>(state, std::bit_xor<Int::type>());
    break;
  }

  default:
  {
    std::ostringstream message;
    message << "TODO execute builtin " << int(id);
    throw std::runtime_error(message.str());
  }

  }

  return 1;

}

void
Builtin::write(std::ostream& stream) const {
  stream << "builtin ";
  for (const auto& pair : BUILTIN) {
    if (pair.second == id) {
      stream << pair.first;
      return;
    }
  }
  stream << "<unknown>";
}
