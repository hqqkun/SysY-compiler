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
                   const std::vector<std::string> argNames,
                   FunctionType *funcType)
    : name(name), funcType(funcType) {
  assert(argNames.size() == funcType->getParamTypes().size() &&
         "Argument names size must match parameter types size");

  for (size_t i = 0; i < argNames.size(); ++i) {
    const std::string &argName = argNames[i];
    Type *paramType = funcType->getParamTypes()[i];
    FuncArg *arg = context.create<FuncArg>(paramType, name, argName, i);
    args.push_back(arg);
  }
}

} // namespace ir
