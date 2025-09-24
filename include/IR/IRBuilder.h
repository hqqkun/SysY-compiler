#ifndef __IR_IRBUILDER_H__
#define __IR_IRBUILDER_H__

#include <cassert>

#include "IR/BasicBlock.h"
#include "IR/Function.h"
#include "IR/Operation.h"
#include "IRContext.h"

namespace ir {

class IRBuilder {
public:
  explicit IRBuilder(IRContext &context)
      : context(context), currentBlock(nullptr), currentFunction(nullptr) {}
  explicit IRBuilder() = delete;
  void setInsertPoint(BasicBlock *block) {
    assert(block && "Basic block cannot be null");
    currentBlock = block;
  }

  template <typename T, typename... Args,
            std::enable_if_t<std::is_base_of_v<Operation, T>, int> = 0>
  T *create(Args &&...args) {
    assert(currentBlock && "No current basic block set for IRBuilder");
    auto *op = context.create<T>(std::forward<Args>(args)...);
    currentBlock->insert_back(op);
    return op;
  }

  IRContext &getContext() { return context; }
  BasicBlock *getCurrentBlock() const { return currentBlock; }
  Function *getCurrentFunction() const { return currentFunction; }

  void setCurrentFunction(Function *func) {
    assert(func && "Function cannot be null");
    currentFunction = func;
  }

  void commitBlock() {
    assert(currentFunction && currentBlock &&
           "Current function or block cannot be null");
    currentFunction->addBlock(currentBlock);
  }

private:
  IRContext &context;
  BasicBlock *currentBlock;
  Function *currentFunction;
};

} // namespace ir

#endif // __IR_IRBUILDER_H__
