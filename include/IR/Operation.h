#ifndef __IR_OPERATION_H__
#define __IR_OPERATION_H__

#include <ostream>
#include <string_view>
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
  virtual std::string_view getOpName() const = 0;
  Type *getResultType() const { return resultType; }
  Value *getOperand(size_t idx) const {
    if (idx >= operands.size())
      return nullptr;
    return operands[idx];
  }
  size_t getNumOperands() const { return operands.size(); }
  bool hasResult() const { return result != nullptr; }
  OpResult *getResult() const { return result; }

protected:
  std::vector<Value *> operands;
  Type *resultType = nullptr;
  OpResult *result = nullptr;
};

class BinaryOp : public Operation {
public:
  explicit BinaryOp(IRContext &context, Value *lhs, Value *rhs);
  virtual std::string_view getOpName() const override = 0;
  Value *getLHS() const { return getOperand(0); }
  Value *getRHS() const { return getOperand(1); }
};

class AddOp : public BinaryOp {
public:
  explicit AddOp(IRContext &context, Value *lhs, Value *rhs);
  std::string_view getOpName() const override { return "add"; }
};

class SubOp : public BinaryOp {
public:
  explicit SubOp(IRContext &context, Value *lhs, Value *rhs);
  std::string_view getOpName() const override { return "sub"; }
};

class EqOp : public BinaryOp {
public:
  explicit EqOp(IRContext &context, Value *lhs, Value *rhs);
  std::string_view getOpName() const override { return "eq"; }
};

class NeqOp : public BinaryOp {
public:
  explicit NeqOp(IRContext &context, Value *lhs, Value *rhs);
  std::string_view getOpName() const override { return "ne"; }
};

class MulOp : public BinaryOp {
public:
  explicit MulOp(IRContext &context, Value *lhs, Value *rhs);
  std::string_view getOpName() const override { return "mul"; }
};

class DivOp : public BinaryOp {
public:
  explicit DivOp(IRContext &context, Value *lhs, Value *rhs);
  std::string_view getOpName() const override { return "div"; }
};

class ModOp : public BinaryOp {
public:
  explicit ModOp(IRContext &context, Value *lhs, Value *rhs);
  std::string_view getOpName() const override { return "mod"; }
};

class LessOp : public BinaryOp {
public:
  explicit LessOp(IRContext &context, Value *lhs, Value *rhs);
  std::string_view getOpName() const override { return "lt"; }
};

class LessEqualOp : public BinaryOp {
public:
  explicit LessEqualOp(IRContext &context, Value *lhs, Value *rhs);
  std::string_view getOpName() const override { return "le"; }
};

class GreaterOp : public BinaryOp {
public:
  explicit GreaterOp(IRContext &context, Value *lhs, Value *rhs);
  std::string_view getOpName() const override { return "gt"; }
};

class GreaterEqualOp : public BinaryOp {
public:
  explicit GreaterEqualOp(IRContext &context, Value *lhs, Value *rhs);
  std::string_view getOpName() const override { return "ge"; }
};

class BitAndOp : public BinaryOp {
public:
  explicit BitAndOp(IRContext &context, Value *lhs, Value *rhs);
  std::string_view getOpName() const override { return "and"; }
};

class BitOrOp : public BinaryOp {
public:
  explicit BitOrOp(IRContext &context, Value *lhs, Value *rhs);
  std::string_view getOpName() const override { return "or"; }
};

class ReturnOp : public Operation {
public:
  explicit ReturnOp(IRContext &context, Value *retVal = nullptr);
  Value *getReturnValue() const { return getOperand(0); }
  std::string_view getOpName() const override { return "ret"; }
};

} // namespace ir

#endif // __IR_OPERATION_H__
