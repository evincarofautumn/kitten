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
  state.push_data<T>(pure(a->value<T>()));
}

template<class T, class F>
void
binary(State& state, F pure) {
  const auto b = state.pop_data();
  const auto a = state.pop_data();
  state.push_data<T>(pure(a->value<T>(), b->value<T>()));
}

template<class T, class F>
void
relational(State& state, F pure) {
  const auto b = state.pop_data();
  const auto a = state.pop_data();
  state.push_data<Bool>(pure(a->value<T>(), b->value<T>()));
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

  case BuiltinId::AddFloat:
    binary<Float>(state, std::plus<Float::type>());
    break;

  case BuiltinId::AddInt:
    binary<Int>(state, std::plus<Int::type>());
    break;

  case BuiltinId::AddVector:
    binary<Vector>(state, add_vector);
    break;

  case BuiltinId::AndBool:
    binary<Bool>(state, std::logical_and<Bool::type>());
    break;

  case BuiltinId::AndInt:
    binary<Int>(state, std::bit_and<Int::type>());
    break;

  case BuiltinId::Apply:
  {
    const auto activation = state.pop_data()->as<Activation>();
    state.call_with_closure(activation.closure, activation.label);
    return 0;
  }

  case BuiltinId::CharToInt:
    state.push_data<Int>(state.pop_data()->value<Char>());
    break;

  // case BuiltinId::Close

  case BuiltinId::DivFloat:
    binary<Float>(state, std::divides<Float::type>());
    break;
  
  case BuiltinId::DivInt:
    binary<Int>(state, std::divides<Int::type>());
    break;

  case BuiltinId::EqFloat:
    relational<Float>(state, std::equal_to<Float::type>());

  case BuiltinId::EqInt:
    relational<Int>(state, std::equal_to<Int::type>());
    break;

  case BuiltinId::Exit:
    state.exit();
    break;

  case BuiltinId::First:
    state.push_data(state.pop_data()->as<Pair>().first);
    break;

  case BuiltinId::FromLeft:
  case BuiltinId::FromRight:
  case BuiltinId::FromSome:
    state.push_data(state.pop_data()->value<Choice>());
    break;

  case BuiltinId::GeFloat:
    relational<Float>(state, std::greater_equal<Float::type>());
    break;

  case BuiltinId::GeInt:
    relational<Int>(state, std::greater_equal<Int::type>());
    break;

  case BuiltinId::Get:
  {
    const auto index = state.pop_data()->value<Int>();
    const auto vector = state.pop_data()->value<Vector>();
    state.push_data(vector.at(index));
    break;
  }

  case BuiltinId::GetLine:
  {
    const auto handle = state.pop_data();
    std::string line;
    std::getline(
      *static_cast<std::istream*>(handle->value<Handle>()),
      line);
    state.push_data(make_char_vector(line));
    break;
  }

  case BuiltinId::GtFloat:
    relational<Float>(state, std::greater<Float::type>());
    break;

  case BuiltinId::GtInt:
    relational<Int>(state, std::greater<Int::type>());
    break;

  case BuiltinId::Impure:
    // Cast
    break;

  case BuiltinId::Init:
  {
    const auto result = state.pop_data()->copy();
    result->value<Vector>().pop_back();
    state.push_data(result);
    break;
  }

  case BuiltinId::LeFloat:
    relational<Float>(state, std::less_equal<Float::type>());
    break;

  case BuiltinId::LeInt:
    relational<Int>(state, std::less_equal<Int::type>());
    break;

  case BuiltinId::Left:
    state.push_data(Choice::make_left(state.pop_data()));
    break;

  case BuiltinId::Length:
    state.push_data<Int>(state.pop_data()->value<Vector>().size());
    break;

  case BuiltinId::LtFloat:
    relational<Float>(state, std::less<Float::type>());
    break;

  case BuiltinId::LtInt:
    relational<Int>(state, std::less<Int::type>());
    break;

  case BuiltinId::ModFloat:
    binary<Float>(
      state,
      static_cast<
        Float::type(*)(Float::type, Float::type)
      >(std::fmod));
    break;

  case BuiltinId::ModInt:
    binary<Int>(state, std::modulus<Int::type>());
    break;

  case BuiltinId::MulFloat:
    binary<Float>(state, std::multiplies<Float::type>());
    break;
  
  case BuiltinId::MulInt:
    binary<Int>(state, std::multiplies<Int::type>());
    break;

  case BuiltinId::NeFloat:
    relational<Float>(state, std::not_equal_to<Float::type>());
    break;

  case BuiltinId::NeInt:
    relational<Int>(state, std::not_equal_to<Int::type>());
    break;

  case BuiltinId::NegFloat:
    unary<Float>(state, std::negate<Float::type>());
    break;

  case BuiltinId::NegInt:
    unary<Int>(state, std::negate<Int::type>());
    break;

  case BuiltinId::None:
    state.push_data<Option>();
    break;

  case BuiltinId::NotBool:
    state.push_data<Bool>(!state.pop_data()->value<Bool>());
    break;

  case BuiltinId::NotInt:
    unary<Int>(state, bit_not<Int::type>());
    break;

  // case BuiltinId::OpenIn
  // case BuiltinId::OpenOut

  case BuiltinId::OrBool:
    relational<Bool>(state, std::logical_or<Bool::type>());
    break;

  case BuiltinId::OrInt:
    binary<Int>(state, std::bit_or<Int::type>());
    break;

  case BuiltinId::Pair:
  {
    const auto b = state.pop_data();
    const auto a = state.pop_data();
    state.push_data<Pair>(a, b);
    break;
  }

  case BuiltinId::Print:
  {
    const auto handle = state.pop_data();
    const auto text = state.pop_data();
    std::string buffer;
    for (const auto& value : text->value<Vector>()) {
      // TODO Use correct character encoding.
      buffer += char(value->value<Char>());
    }
    *static_cast<std::ostream*>(handle->value<Handle>())
      << buffer;
    break;
  }

  case BuiltinId::Rest:
    state.push_data(state.pop_data()->as<Pair>().second);
    break;

  case BuiltinId::Right:
    state.push_data(Choice::make_right(state.pop_data()));
    break;

  case BuiltinId::Set:
  {
    const auto index = state.pop_data()->value<Int>();
    const auto value = state.pop_data();
    const auto vector = state.pop_data()->copy();
    vector->value<Vector>().at(index) = value;
    state.push_data(vector);
    break;
  }

  case BuiltinId::ShowFloat:
  {
    const auto a = state.pop_data();
    std::ostringstream stream;
    stream << a->value<Float>();
    state.push_data(make_char_vector(stream.str()));
    break;
  }

  case BuiltinId::ShowInt:
  {
    const auto a = state.pop_data();
    std::ostringstream stream;
    stream << a->value<Int>();
    state.push_data(make_char_vector(stream.str()));
    break;
  }

  case BuiltinId::Some:
    state.push_data<Option>(state.pop_data());
    break;

  case BuiltinId::Stderr:
    state.push_data(Handle::stderr);
    break;

  case BuiltinId::Stdin:
    state.push_data(Handle::stdin);
    break;

  case BuiltinId::Stdout:
    state.push_data(Handle::stdout);
    break;

  case BuiltinId::SubFloat:
    binary<Float>(state, std::minus<Float::type>());
    break;

  case BuiltinId::SubInt:
    binary<Int>(state, std::minus<Int::type>());
    break;

  case BuiltinId::Tail:
  {
    const auto result = state.pop_data()->copy();
    auto& vector = result->value<Vector>();
    vector.erase(begin(vector));
    state.push_data(result);
    break;
  }

  case BuiltinId::UnsafePurify11:
    // Cast
    break;

  case BuiltinId::XorBool:
    binary<Int>(state, std::bit_xor<Int::type>());
    break;

  case BuiltinId::XorInt:
    binary<Int>(state, std::bit_xor<Int::type>());
    break;

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
