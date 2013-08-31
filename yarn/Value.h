#ifndef VALUE_H_9D13EAFF_9778_4CFF_8B9A_EB422A7032DA
#define VALUE_H_9D13EAFF_9778_4CFF_8B9A_EB422A7032DA

#include <iosfwd>
#include <memory>

struct Value {

  virtual ~Value() {}

  friend std::ostream& operator<<(std::ostream&, const Value&);

  template<class T>
  T& as() {
    return dynamic_cast<T&>(*this);
  }

  template<class T>
  const T& as() const {
    return dynamic_cast<const T&>(*this);
  }

  template<class T>
  typename T::type& value() {
    return as<T>().value;
  }

  template<class T>
  const typename T::type& value() const {
    return as<T>().value;
  }

  virtual std::shared_ptr<Value> copy() const = 0;
  virtual void write(std::ostream& stream) const = 0;

};

typedef std::shared_ptr<Value> MutableValuePtr;
typedef std::shared_ptr<Value> ValuePtr;

#endif
