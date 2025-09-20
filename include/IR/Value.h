#ifndef __IR_VALUE_H__
#define __IR_VALUE_H__

#include <ostream>
#include <string>
#include <unordered_set>

#include "IR/IRContext.h"
#include "IR/Object.h"
#include "IR/Operation.h"
#include "IR/Type.h"

namespace ir {
class Operation;
class Value : public IRObject {
public:
  explicit Value() = delete;
  virtual ~Value() = default;
  Type *getType() const { return _type; }
  bool isInteger() const { return getKind() == ValueKind::kInteger; }
  bool isOpResult() const { return getKind() == ValueKind::kOpResult; }
  bool isFuncArg() const { return getKind() == ValueKind::kFuncArg; }
  const std::unordered_set<Operation *> &getUsers() const { return users; }
  std::unordered_set<Operation *> &getUsers() { return users; }
  void addUser(Operation *u) { users.insert(u); }

protected:
  enum class ValueKind { kInvalid, kInteger, kOpResult, kFuncArg };
  explicit Value(Type *type = nullptr, ValueKind kind = ValueKind::kInvalid)
      : _type(type), _kind(kind) {}
  ValueKind getKind() const { return _kind; }
  Type *_type;
  ValueKind _kind;
  // TODO: Support users of this value.
  std::unordered_set<Operation *> users;
};

class Integer : public Value {
public:
  explicit Integer(IRContext &context, int val, Type *type)
      : Value(type, ValueKind::kInteger), integer(val) {}
  int getValue() const { return integer; }

  static Integer *get(IRContext &context, int val, unsigned int bitwidth = 32);

private:
  int integer;
};

class OpResult : public Value {
public:
  explicit OpResult(IRContext &context, Operation *defOp)
      : Value(defOp->getResultType(), ValueKind::kOpResult), defOp(defOp) {}
  Operation *getDefiningOp() const { return defOp; }

private:
  Operation *defOp;
};

class FuncArg : public Value {
public:
  explicit FuncArg(IRContext &context, Type *type, size_t index)
      : Value(type, ValueKind::kFuncArg), index(index) {}
  size_t getIndex() const { return index; }

private:
  std::string name; // argument name
  size_t index;     // argument index in the function
};

} // namespace ir

#endif // __IR_VALUE_H__
