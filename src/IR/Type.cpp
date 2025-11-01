#include <cassert>
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

FunctionType *FunctionType::get(IRContext &context,
                                std::optional<Type *> returnType,
                                const std::vector<Type *> &paramTypes) {
  return context.create<FunctionType>(returnType, paramTypes);
}

PointerType *PointerType::get(IRContext &context, Type *pointeeType) {
  return context.create<PointerType>(pointeeType);
}

ArrayType *ArrayType::get(IRContext &context, Type *elementType, size_t size) {
  return context.create<ArrayType>(elementType, size);
}

ArrayType *ArrayType::get(IRContext &context, Type *elementType,
                          const std::vector<size_t> &dims) {
  assert(!dims.empty() && "Array must have at least one dimension");
  Type *currentType = elementType;
  for (auto it = dims.rbegin(); it != dims.rend(); ++it) {
    currentType = ArrayType::get(context, currentType, *it);
  }
  return static_cast<ArrayType *>(currentType);
}

} // namespace ir
