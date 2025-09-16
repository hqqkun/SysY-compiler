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

SubOp::SubOp(IRContext &context, Value *lhs, Value *rhs)
    : BinaryOp(context, lhs, rhs) {
  resultType = lhs->getType();
  result = context.create<OpResult>(this);
}

MulOp::MulOp(IRContext &context, Value *lhs, Value *rhs)
    : BinaryOp(context, lhs, rhs) {
  resultType = lhs->getType();
  result = context.create<OpResult>(this);
}

DivOp::DivOp(IRContext &context, Value *lhs, Value *rhs)
    : BinaryOp(context, lhs, rhs) {
  resultType = lhs->getType();
  result = context.create<OpResult>(this);
}

ModOp::ModOp(IRContext &context, Value *lhs, Value *rhs)
    : BinaryOp(context, lhs, rhs) {
  resultType = lhs->getType();
  result = context.create<OpResult>(this);
}

EqOp::EqOp(IRContext &context, Value *lhs, Value *rhs)
    : BinaryOp(context, lhs, rhs) {
  // For accuracy, we assume comparison results in a boolean type.
  resultType = IntegerType::get(context, 1);
  result = context.create<OpResult>(this);
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

} // namespace ir
