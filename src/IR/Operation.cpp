#include <cassert>
#include <ostream>
#include <string>
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
  // Add user for lhs and rhs.
  lhs->addUser(this);
  rhs->addUser(this);
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

NeqOp::NeqOp(IRContext &context, Value *lhs, Value *rhs)
    : BinaryOp(context, lhs, rhs) {
  resultType = IntegerType::get(context, 1);
  result = context.create<OpResult>(this);
}

LessOp::LessOp(IRContext &context, Value *lhs, Value *rhs)
    : BinaryOp(context, lhs, rhs) {
  resultType = IntegerType::get(context, 1);
  result = context.create<OpResult>(this);
}

LessEqualOp::LessEqualOp(IRContext &context, Value *lhs, Value *rhs)
    : BinaryOp(context, lhs, rhs) {
  resultType = IntegerType::get(context, 1);
  result = context.create<OpResult>(this);
}

GreaterOp::GreaterOp(IRContext &context, Value *lhs, Value *rhs)
    : BinaryOp(context, lhs, rhs) {
  resultType = IntegerType::get(context, 1);
  result = context.create<OpResult>(this);
}

GreaterEqualOp::GreaterEqualOp(IRContext &context, Value *lhs, Value *rhs)
    : BinaryOp(context, lhs, rhs) {
  resultType = IntegerType::get(context, 1);
  result = context.create<OpResult>(this);
}

BitAndOp::BitAndOp(IRContext &context, Value *lhs, Value *rhs)
    : BinaryOp(context, lhs, rhs) {
  resultType = IntegerType::get(context, 32);
  result = context.create<OpResult>(this);
}

BitOrOp::BitOrOp(IRContext &context, Value *lhs, Value *rhs)
    : BinaryOp(context, lhs, rhs) {
  resultType = IntegerType::get(context, 32);
  result = context.create<OpResult>(this);
}

ReturnOp::ReturnOp(IRContext &context, Value *retVal) {
  if (retVal) {
    operands.emplace_back(retVal);
    retVal->addUser(this);
    resultType = retVal->getType();
  } else {
    resultType = VoidType::get(context);
  }
  // ReturnOp does not produce a result
  result = nullptr;
}

AllocOp::AllocOp(IRContext &context, const std::string &name, Type *allocType,
                 size_t size)
    : allocSize(size), varName(name), elemType(allocType) {
  assert(allocType && "AllocOp type cannot be null");
  resultType = PointerType::get(context, allocType);
  result = context.create<OpResult>(this);
}

LoadOp::LoadOp(IRContext &context, Value *ptr) {
  assert(ptr && "LoadOp pointer cannot be null");
  operands.emplace_back(ptr);
  ptr->addUser(this);
  // The result type is the pointee type of the pointer.
  if (auto *ptrType = dynamic_cast<PointerType *>(ptr->getType())) {
    resultType = ptrType->getPointeeType();
  } else {
    assert(false && "LoadOp operand is not a pointer type");
  }
  result = context.create<OpResult>(this);
}

StoreOp::StoreOp(IRContext &context, Value *val, Value *ptr) {
  assert(val && "StoreOp value cannot be null");
  assert(ptr && "StoreOp pointer cannot be null");
  assert(ptr->getType()->isPointer() &&
         "StoreOp pointer must be a pointer type");
  operands.emplace_back(val);
  operands.emplace_back(ptr);
  val->addUser(this);
  ptr->addUser(this);
  resultType = VoidType::get(context);
  result = nullptr;
}

BranchOp::BranchOp(IRContext &context, Value *cond, Value *thenArg,
                   Value *elseArg) {
  assert(cond && "BranchOp condition cannot be null");
  assert(thenArg && "BranchOp then argument cannot be null");
  assert(elseArg && "BranchOp else argument cannot be null");
  // Check that thenArg and elseArg are `JumpArg`.
  assert(thenArg->isJumpArg() && "BranchOp then argument must be a JumpArg");
  assert(elseArg->isJumpArg() && "BranchOp else argument must be a JumpArg");
  assert(cond->getType()->isInteger() &&
         "BranchOp condition must be an integer type");
  operands.emplace_back(cond);
  operands.emplace_back(thenArg);
  operands.emplace_back(elseArg);
  cond->addUser(this);
  thenArg->addUser(this);
  elseArg->addUser(this);
  result = nullptr;
  // BranchOp has no result type.
}

JumpOp::JumpOp(IRContext &context, Value *targetArg) {
  assert(targetArg && "JumpOp target argument cannot be null");
  assert(targetArg->isJumpArg() && "JumpOp target argument must be a JumpArg");
  operands.emplace_back(targetArg);
  targetArg->addUser(this);
  result = nullptr;
  // JumpOp has no result type.
}

} // namespace ir
