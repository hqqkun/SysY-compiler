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
                    const std::vector<std::string> argNames,
                    FunctionType *funcType);
  BasicBlock *getEntryBlock() const;
  FuncArg *getArg(size_t index) const;
  size_t getNumArgs() const { return args.size(); }
  const std::vector<FuncArg *> &getArgs() const { return args; }
  bool hasArgs() const { return !args.empty(); }
  void addBlock(BasicBlock *block) {
    if (block) {
      blocks.push_back(block);
    }
  }

  static Function *create(IRContext &context, const std::string &name,
                          const std::vector<std::string> argNames,
                          FunctionType *funcType) {
    return context.create<Function>(name, argNames, funcType);
  }

  FunctionType *getFunctionType() const { return funcType; }
  const std::string &getName() const { return name; }

  using block_iterator = std::list<BasicBlock *>::iterator;
  using const_block_iterator = std::list<BasicBlock *>::const_iterator;

  size_t size() const { return blocks.size(); }
  block_iterator begin() { return blocks.begin(); }
  const_block_iterator begin() const { return blocks.begin(); }
  block_iterator end() { return blocks.end(); }
  const_block_iterator end() const { return blocks.end(); }

private:
  std::string name;
  std::list<BasicBlock *> blocks;
  std::vector<FuncArg *> args;
  FunctionType *funcType;
};
} // namespace ir

#endif // __IR_FUNCTION_H__
