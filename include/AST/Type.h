#ifndef __AST_TYPE_H__
#define __AST_TYPE_H__

#include <string>

namespace ast {

enum class BaseType {
  INT,
  VOID,
};

struct Type {
  enum class Kind { BASE, POINTER };
  Kind kind;

  union {
    BaseType base;
    const Type *pointee; // For pointer type.
  };

  explicit Type(BaseType b) : kind(Kind::BASE), base(b) {}
  explicit Type(const Type *p) : kind(Kind::POINTER), pointee(p) {}
  bool isBase() const { return kind == Kind::BASE; }
  bool isPointer() const { return kind == Kind::POINTER; }
};

std::string toString(const Type &type);

} // namespace ast

#endif // __AST_TYPE_H__
