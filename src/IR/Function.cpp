#include <iostream>
#include <vector>

#include "IR/BasicBlock.h"
#include "IR/Function.h"

namespace ir {

BasicBlock *Function::getEntryBlock() const {
  if (blocks.empty())
    return nullptr;
  return blocks.front();
}

FuncArg *Function::getArg(size_t index) const {
  if (index >= args.size())
    return nullptr;
  return args[index];
}

Function::Function(IRContext &context, const std::string &name,
                   FunctionType *funcType)
    : name(name), funcType(funcType) {
  for (Type *paramType : funcType->getParamTypes()) {
    size_t index = args.size();
    FuncArg *arg = context.create<FuncArg>(paramType, index);
    args.push_back(arg);
  }
}

} // namespace ir
