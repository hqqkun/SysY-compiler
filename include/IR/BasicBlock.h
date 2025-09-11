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

  void print(std::ostream &os) const override;

private:
  std::list<Operation *> operations;
  std::string name;
};
} // namespace ir

#endif // __IR_BASICBLOCK_H__
