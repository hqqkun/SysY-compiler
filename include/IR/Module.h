#ifndef __IR_MODULE_H__
#define __IR_MODULE_H__

#include <list>

#include "IR/Function.h"

namespace ir {

class Module : public IRObject {
public:
  explicit Module(IRContext &context) {}
  void addFunction(Function *func) {
    if (func) {
      functions.push_back(func);
    }
  }

  static Module *create(IRContext &context) { return context.create<Module>(); }

  using func_iterator = std::list<Function *>::iterator;
  using const_func_iterator = std::list<Function *>::const_iterator;
  size_t size() const { return functions.size(); }
  func_iterator begin() { return functions.begin(); }
  const_func_iterator begin() const { return functions.begin(); }
  func_iterator end() { return functions.end(); }
  const_func_iterator end() const { return functions.end(); }

private:
  // TODO: Add global variables, etc.
  std::list<Function *> functions;
};

} // namespace ir

#endif // __IR_MODULE_H__
