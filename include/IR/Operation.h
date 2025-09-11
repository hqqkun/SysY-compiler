#ifndef __IR_OPERATION_H__
#define __IR_OPERATION_H__

#include <memory>
#include <ostream>
#include <vector>

#include "IR/IRContext.h"
#include "IR/Object.h"
#include "IR/Type.h"

namespace ir {

class Value;
class OpResult;

class Operation : public IRObject {
public:
  explicit Operation() : resultType(nullptr), result(nullptr) {}
  virtual ~Operation() = default;
  virtual void print(std::ostream &os) const = 0;
  Type *getResultType() const { return resultType; }
  Value *getOperand(size_t idx) const {
    if (idx >= operands.size())
      return nullptr;
    return operands[idx];
  }
  size_t getNumOperands() const { return operands.size(); }

protected:
  std::vector<Value *> operands;
  Type *resultType;
  OpResult *result;
};

class BinaryOp : public Operation {
public:
  explicit BinaryOp(IRContext &context, Value *lhs, Value *rhs);
  virtual void print(std::ostream &os) const override = 0;
  Value *getLHS() const { return getOperand(0); }
  Value *getRHS() const { return getOperand(1); }
};

class AddOp : public BinaryOp {
public:
  explicit AddOp(IRContext &context, Value *lhs, Value *rhs);
  void print(std::ostream &os) const override;
};

class ReturnOp : public Operation {
public:
  explicit ReturnOp(IRContext &context, Value *retVal = nullptr);
  void print(std::ostream &os) const override;
  Value *getReturnValue() const { return getOperand(0); }
};

} // namespace ir

#endif // __IR_OPERATION_H__
