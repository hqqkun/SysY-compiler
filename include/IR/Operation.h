#ifndef __IR_OPERATION_H__
#define __IR_OPERATION_H__

#include <ostream>
#include <string>
#include <string_view>
#include <vector>

#include "IR/IRContext.h"
#include "IR/Object.h"
#include "IR/Type.h"

namespace ir {

class Value;
class OpResult;
class BasicBlock;

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
  virtual bool isTerminator() const { return false; }

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
  bool isTerminator() const override { return true; }
};

class AllocOp : public Operation {
public:
  explicit AllocOp(IRContext &context, const std::string &var, Type *allocType,
                   bool isUserVar = true, size_t size = 1);
  std::string_view getOpName() const override { return "alloc"; }
  const std::string &getVarName() const { return varName; }
  Type *getAllocType() const { return elemType; }
  bool isUserVariable() const { return userVariable; }

private:
  /// Indicates whether this allocation is for a user-defined variable.
  /// If false, it may be for a temporary or internal use.
  bool userVariable;
  size_t allocSize;
  std::string varName;
  Type *elemType;
};

class LoadOp : public Operation {
public:
  explicit LoadOp(IRContext &context, Value *ptr);
  Value *getPointer() const { return getOperand(0); }
  std::string_view getOpName() const override { return "load"; }
};

class StoreOp : public Operation {
public:
  explicit StoreOp(IRContext &context, Value *val, Value *ptr);
  Value *getValue() const { return getOperand(0); }
  Value *getPointer() const { return getOperand(1); }
  std::string_view getOpName() const override { return "store"; }
};

class BranchOp : public Operation {
public:
  /**
   * Returns the name of the branch operation.
   * Implementers should provide a unique string identifier for the specific
   * branch operation. This method is pure virtual to ensure all branch
   * operations define their own name.
   */
  std::string_view getOpName() const override = 0;
  bool isTerminator() const override { return true; }
};

class CondBranchOp : public BranchOp {
public:
  explicit CondBranchOp(IRContext &context, Value *cond, BasicBlock *thenBlock,
                        BasicBlock *elseBlock);
  Value *getCondition() const { return getOperand(0); }
  BasicBlock *getThenBB() const { return thenBB; }
  BasicBlock *getElseBB() const { return elseBB; }

  std::string_view getOpName() const override { return "br"; }

private:
  BasicBlock *thenBB;
  BasicBlock *elseBB;
};

class JumpOp : public BranchOp {
public:
  explicit JumpOp(IRContext &context, BasicBlock *target);
  BasicBlock *getTargetBB() const { return targetBB; }

  std::string_view getOpName() const override { return "jump"; }

private:
  BasicBlock *targetBB;
};

} // namespace ir

#endif // __IR_OPERATION_H__
