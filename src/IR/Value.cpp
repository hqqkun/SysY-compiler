#include <ostream>

#include "IR/IRContext.h"
#include "IR/Object.h"
#include "IR/Operation.h"
#include "IR/Type.h"
#include "IR/Value.h"

namespace ir {

/// Static factory method to create Integer values using IRContext.
Integer *Integer::get(IRContext &context, int val, unsigned int bitwidth) {
  Type *type = IntegerType::get(context, bitwidth);
  return context.create<Integer>(val, type);
}

/// Static factory method to create JumpArg values using IRContext.
JumpArg *JumpArg::get(IRContext &context, BasicBlock *bb) {
  return context.create<JumpArg>(bb);
}

} // namespace ir
