#ifndef __IR_FUNCTION_H__
#define __IR_FUNCTION_H__

#include <list>
#include <ostream>
#include <string>
#include <vector>

#include "IR/BasicBlock.h"
#include "IR/IRContext.h"
#include "IR/Object.h"
#include "IR/Value.h"

namespace ir {
class Function : public IRObject {
public:
  explicit Function(IRContext &context, const std::string &name,
                    FunctionType *funcType);
  BasicBlock *getEntryBlock() const;
  FuncArg *getArg(size_t index) const;
  size_t getNumArgs() const { return args.size(); }
  void addBlock(BasicBlock *block) {
    if (block) {
      blocks.push_back(block);
    }
  }
  FunctionType *getFunctionType() const { return funcType; }
  const std::string &getName() const { return name; }
  void print(std::ostream &os) const override;

private:
  std::string name;
  std::list<BasicBlock *> blocks;
  std::vector<FuncArg *> args;
  FunctionType *funcType;
};
} // namespace ir

#endif // __IR_FUNCTION_H__
