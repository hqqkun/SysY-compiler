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

} // namespace ir
