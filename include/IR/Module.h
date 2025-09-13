#ifndef __IR_MODULE_H__
#define __IR_MODULE_H__

#include <list>

#include "IR/Function.h"

namespace ir {

class Module : public IRObject {
public:
  explicit Module() = default;
  void addFunction(Function *func) {
    if (func) {
      functions.push_back(func);
    }
  }

  void print(std::ostream &os) const override {}

private:
  // TODO: Add global variables, etc.
  std::list<Function *> functions;
};

} // namespace ir

#endif // __IR_MODULE_H__
