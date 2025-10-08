#ifndef __IR_TYPE_H__
#define __IR_TYPE_H__

#include <cstddef>
#include <optional>
#include <ostream>
#include <vector>

#include "IR/IRContext.h"
#include "IR/Object.h"

namespace ir {

class Type : public IRObject {
public:
  explicit Type() = delete;
  virtual ~Type() = default;
  Type(const Type &) = delete;
  Type &operator=(const Type &) = delete;

  bool isInteger() const { return kind == TypeKind::kInteger; }
  bool isVoid() const { return kind == TypeKind::kVoid; }
  bool isFunction() const { return kind == TypeKind::kFunction; }
  bool isValid() const { return kind != TypeKind::kInvalid; }
  bool isPointer() const { return kind == TypeKind::KPointer; }

protected:
  enum class TypeKind { kInvalid, kInteger, kVoid, kFunction, KPointer };
  explicit Type(TypeKind k = TypeKind::kInvalid) : kind(k) {}
  TypeKind kind;
};

class IntegerType : public Type {
public:
  explicit IntegerType(IRContext &context, unsigned int bitWidth = 32)
      : Type(TypeKind::kInteger), bitWidth(bitWidth) {}
  unsigned int getBitWidth() const { return bitWidth; }

  static IntegerType *get(IRContext &context, unsigned int bitWidth = 32);

private:
  unsigned int bitWidth;
};

class VoidType : public Type {
public:
  explicit VoidType(IRContext &context) : Type(TypeKind::kVoid) {}

  static VoidType *get(IRContext &context);
};

class FunctionType : public Type {
public:
  explicit FunctionType(IRContext &context, std::optional<Type *> returnType,
                        const std::vector<Type *> &paramTypes)
      : Type(TypeKind::kFunction), _returnType(returnType),
        paramTypes(paramTypes) {}
  Type *getReturnType() const { return _returnType.value_or(nullptr); }
  bool hasReturnType() const { return _returnType.has_value(); }
  const std::vector<Type *> &getParamTypes() const { return paramTypes; }

  static FunctionType *get(IRContext &context, std::optional<Type *> returnType,
                           const std::vector<Type *> &paramTypes);

private:
  std::optional<Type *> _returnType;
  std::vector<Type *> paramTypes;
};

class PointerType : public Type {
public:
  explicit PointerType(IRContext &context, Type *pointeeType)
      : Type(TypeKind::KPointer), pointeeType(pointeeType) {}
  Type *getPointeeType() const { return pointeeType; }

  static PointerType *get(IRContext &context, Type *pointeeType);

private:
  Type *pointeeType;
};

} // namespace ir

#endif // __IR_TYPE_H__
