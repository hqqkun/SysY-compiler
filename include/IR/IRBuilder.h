#ifndef __IR_IRBUILDER_H__
#define __IR_IRBUILDER_H__

#include "IR/BasicBlock.h"
#include "IR/Function.h"
#include "IR/Operation.h"
#include "IRContext.h"

namespace ir {

class IRBuilder {
public:
  explicit IRBuilder(IRContext &context)
      : context(context), currentBlock(nullptr) {}
  explicit IRBuilder() = delete;
  void setInsertPoint(BasicBlock *block) { currentBlock = block; }

  template <typename T, typename... Args,
            std::enable_if_t<std::is_base_of_v<Operation, T>, int> = 0>
  T *create(Args &&...args) {
    if (currentBlock) {
      auto *op = context.create<T>(std::forward<Args>(args)...);
      currentBlock->insert_back(op);
      return op;
    } else {
      throw std::runtime_error("No current basic block set for IRBuilder");
    }
  }

  IRContext &getContext() { return context; }

private:
  IRContext &context;
  BasicBlock *currentBlock;
};

} // namespace ir

#endif // __IR_IRBUILDER_H__
