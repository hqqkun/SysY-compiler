#ifndef __IR_TYPE_H__
#define __IR_TYPE_H__

#include <ostream>
#include <vector>

#include "IR/IRContext.h"
#include "IR/Object.h"

namespace ir {

class Type : public IRObject {
public:
  explicit Type() = default;
  virtual ~Type() = default;
  virtual void print(std::ostream &os) const override = 0;
  Type(const Type &) = delete;
  Type &operator=(const Type &) = delete;
};

class IntegerType : public Type {
public:
  explicit IntegerType(IRContext &context, unsigned int bitWidth = 32)
      : bitWidth(bitWidth) {}
  unsigned int getBitWidth() const { return bitWidth; }
  void print(std::ostream &os) const override;

  static IntegerType *get(IRContext &context, unsigned int bitWidth = 32);

private:
  unsigned int bitWidth;
};

class VoidType : public Type {
public:
  explicit VoidType(IRContext &context) {}
  void print(std::ostream &os) const override;

  static VoidType *get(IRContext &context);
};

class FunctionType : public Type {
public:
  explicit FunctionType(IRContext &context, Type *returnType,
                        const std::vector<Type *> &paramTypes)
      : _returnType(returnType), paramTypes(paramTypes) {}
  Type *getReturnType() const { return _returnType; }
  const std::vector<Type *> &getParamTypes() const { return paramTypes; }
  void print(std::ostream &os) const override;

  static FunctionType *get(IRContext &context, Type *returnType,
                           const std::vector<Type *> &paramTypes);

private:
  Type *_returnType;
  std::vector<Type *> paramTypes;
};

} // namespace ir

#endif // __IR_TYPE_H__
