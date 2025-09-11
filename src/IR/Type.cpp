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

/// Print methods for types.
void IntegerType::print(std::ostream &os) const { os << "i" << bitWidth; }

void VoidType::print(std::ostream &os) const {
  // TODO: implement VoidType printing.
  throw std::runtime_error("VoidType::print not implemented");
}

void FunctionType::print(std::ostream &os) const {
  // Print parameter types.
  os << "(";
  for (size_t i = 0; i < paramTypes.size(); ++i) {
    if (i > 0) {
      os << ", ";
    }
    paramTypes[i]->print(os);
  }
  os << ")";

  // Print return type.
  if (dynamic_cast<VoidType *>(_returnType)) {
    return;
  }

  os << ": ";
  _returnType->print(os);
}

} // namespace ir
