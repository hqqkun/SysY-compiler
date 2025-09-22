#include <cstddef>
#include <iostream>
#include <ostream>

#include "IR/IRContext.h"
#include "IR/Type.h"

namespace ir {

/// Static factory methods to create types using IRContext.
IntegerType *IntegerType::get(IRContext &context, unsigned int bitWidth) {
  return context.create<IntegerType>(bitWidth);
}

VoidType *VoidType::get(IRContext &context) {
  return context.create<VoidType>();
}

FunctionType *FunctionType::get(IRContext &context, Type *returnType,
                                const std::vector<Type *> &paramTypes) {
  return context.create<FunctionType>(returnType, paramTypes);
}

PointerType *PointerType::get(IRContext &context, Type *pointeeType) {
  return context.create<PointerType>(pointeeType);
}

} // namespace ir
