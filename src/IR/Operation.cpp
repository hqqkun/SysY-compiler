#include <cassert>
#include <ostream>
#include <vector>

#include "IR/Operation.h"
#include "IR/Type.h"
#include "IR/Value.h"

namespace ir {

BinaryOp::BinaryOp(IRContext &context, Value *lhs, Value *rhs) {
  assert(lhs && "BinaryOp lhs cannot be null");
  assert(rhs && "BinaryOp rhs cannot be null");
  operands.emplace_back(lhs);
  operands.emplace_back(rhs);
}

AddOp::AddOp(IRContext &context, Value *lhs, Value *rhs)
    : BinaryOp(context, lhs, rhs) {
  resultType = lhs->getType();
  result = context.create<OpResult>(this);
}

void AddOp::print(std::ostream &os) const {
  // TODO: implement AddOp printing
  throw std::runtime_error("AddOp::print not implemented");
}

ReturnOp::ReturnOp(IRContext &context, Value *retVal) {
  if (retVal) {
    operands.emplace_back(retVal);
    resultType = retVal->getType();
  } else {
    resultType = VoidType::get(context);
  }
  // ReturnOp does not produce a result
  result = nullptr;
}

void ReturnOp::print(std::ostream &os) const {
  os << "ret";
  if (getReturnValue()) {
    os << " ";
    getReturnValue()->print(os);
  }
  os << std::endl;
}
} // namespace ir
