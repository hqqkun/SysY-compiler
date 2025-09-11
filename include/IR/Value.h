#ifndef __IR_VALUE_H__
#define __IR_VALUE_H__

#include <ostream>
#include <string>

#include "IR/IRContext.h"
#include "IR/Object.h"
#include "IR/Operation.h"
#include "IR/Type.h"

namespace ir {
class Operation;

class Value : public IRObject {
public:
  explicit Value() : _type(nullptr) {}
  explicit Value(Type *type) : _type(type) {}
  virtual ~Value() = default;
  virtual void print(std::ostream &os) const override = 0;
  Type *getType() const { return _type; }

protected:
  Type *_type;
};

class Integer : public Value {
public:
  explicit Integer(IRContext &context, int val, Type *type)
      : Value(type), integer(val) {}
  int getValue() const { return integer; }
  void print(std::ostream &os) const override;

  static Integer *get(IRContext &context, int val, unsigned int bitwidth = 32);

private:
  int integer;
};

class OpResult : public Value {
public:
  explicit OpResult(IRContext &context, Operation *defOp)
      : Value(defOp->getResultType()), defOp(defOp) {}
  Operation *getDefiningOp() const { return defOp; }
  void print(std::ostream &os) const override;

private:
  Operation *defOp;
};

class FuncArg : public Value {
public:
  explicit FuncArg(IRContext &context, Type *type, size_t index)
      : Value(type), index(index) {}
  size_t getIndex() const { return index; }
  void print(std::ostream &os) const override;

private:
  std::string name; // argument name
  size_t index;     // argument index in the function
};

} // namespace ir

#endif // __IR_VALUE_H__
