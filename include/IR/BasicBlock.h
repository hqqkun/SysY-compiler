#ifndef __IR_BASICBLOCK_H__
#define __IR_BASICBLOCK_H__

#include <list>
#include <ostream>
#include <string>

#include "IR/IRContext.h"
#include "IR/Object.h"
#include "IR/Operation.h"

namespace ir {
class BasicBlock : public IRObject {
public:
  virtual ~BasicBlock() = default;
  explicit BasicBlock(IRContext &context, const std::string &name)
      : name(name) {}
  void insert_back(Operation *op) {
    if (op) {
      operations.push_back(op);
    }
  }

  void insert_front(Operation *op) {
    if (op) {
      operations.push_front(op);
    }
  }
  const std::string &getName() const { return name; }

  static BasicBlock *create(IRContext &context, const std::string &name) {
    return context.create<BasicBlock>(name);
  }

  using iterator = std::list<Operation *>::iterator;
  using const_iterator = std::list<Operation *>::const_iterator;

  size_t size() const { return operations.size(); }
  iterator begin() { return operations.begin(); }
  const_iterator begin() const { return operations.begin(); }
  iterator end() { return operations.end(); }
  const_iterator end() const { return operations.end(); }

private:
  std::list<Operation *> operations;
  std::string name;
};
} // namespace ir

#endif // __IR_BASICBLOCK_H__
