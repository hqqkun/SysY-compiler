#ifndef __AST_TYPE_H__
#define __AST_TYPE_H__

#include <memory>
#include <string>
#include <vector>

namespace ast {
class ConstExpAST;
using ConstExpPtr = std::unique_ptr<class ConstExpAST>;

enum class BaseType {
  INT,
  VOID,
};

struct Type {
  enum class Kind { BASE, POINTER, ARRAY };
  Kind kind;

  union {
    BaseType base;
    const Type *pointee; // For pointer type.
  };

  struct {
    const Type *elementType;
    std::unique_ptr<std::vector<ConstExpPtr>> sizes;
  } array; // For array type.

  virtual ~Type();

  explicit Type(BaseType b) : kind(Kind::BASE), base(b) {}
  explicit Type(const Type *p) : kind(Kind::POINTER), pointee(p) {}
  explicit Type(const Type *elemType,
                std::unique_ptr<std::vector<ConstExpPtr>> sizes)
      : kind(Kind::ARRAY) {
    array.elementType = elemType;
    array.sizes = std::move(sizes);
  }
  bool isBase() const { return kind == Kind::BASE; }
  bool isPointer() const { return kind == Kind::POINTER; }
  bool isArray() const { return kind == Kind::ARRAY; }
};

std::string toString(const Type &type);

} // namespace ast

#endif // __AST_TYPE_H__
