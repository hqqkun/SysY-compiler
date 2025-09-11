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

/// Print the integer value to the output stream.
void Integer::print(std::ostream &os) const { os << integer; }

void OpResult::print(std::ostream &os) const {
  // TODO: implement OpResult printing.
  throw std::runtime_error("OpResult::print not implemented");
}
void FuncArg::print(std::ostream &os) const {
  // TODO: implement FuncArg printing.
  throw std::runtime_error("FuncArg::print not implemented");
}

} // namespace ir
