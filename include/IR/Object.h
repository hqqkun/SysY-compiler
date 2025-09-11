#ifndef __IR_OBJECT_H__
#define __IR_OBJECT_H__

#include <ostream>

namespace ir {
class IRObject {
public:
  virtual ~IRObject() = default;
  virtual void print(std::ostream &os) const = 0;
};
} // namespace ir

#endif // __IR_OBJECT_H__
