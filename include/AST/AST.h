#ifndef __AST_AST_H__
#define __AST_AST_H__

#include <memory>
#include <string>
#include <string_view>

#include "AST/Ops.h"
#include "AST/Type.h"

namespace ast {
using ASTPtr = std::unique_ptr<class BaseAST>;
class BaseAST {
public:
  virtual ~BaseAST() = default;
  virtual void dump() const = 0;
};

class CompUnitAST : public BaseAST {
public:
  ASTPtr funcDef;
  void dump() const override;
};

class FuncDefAST : public BaseAST {
public:
  ASTPtr funcType;
  std::string ident;
  ASTPtr block;
  void dump() const override;
};

class FuncTypeAST : public BaseAST {
public:
  Type type;
  void dump() const override;
};

class BlockAST : public BaseAST {
public:
  ASTPtr stmt;
  void dump() const override;
};

class StmtAST : public BaseAST {
public:
  ASTPtr exp;
  void dump() const override;
};

class ExprAST : public BaseAST {
public:
  ASTPtr addExp;
  void dump() const override;
};

// PrimaryExp ::= "(" Exp ")" | Number
class PrimaryExpAST : public BaseAST {
public:
  int number;
  ASTPtr exp;
  void dump() const override;

  PrimaryExpAST(int num) : number(num), type(Type::NUMBER) {}
  PrimaryExpAST(ASTPtr e) : exp(std::move(e)), type(Type::EXP) {}
  bool isNumber() const { return type == Type::NUMBER; }
  bool isExp() const { return type == Type::EXP; }

private:
  enum class Type { NUMBER, EXP } type;
};

// UnaryExp ::= PrimaryExp | UnaryOp UnaryExp
class UnaryExpAST : public BaseAST {
public:
  ASTPtr primaryExp;
  Op unaryOp;
  ASTPtr childUnaryExp;
  void dump() const override;

  UnaryExpAST(ASTPtr primary)
      : primaryExp(std::move(primary)), type(Type::PRIMARY) {}

  UnaryExpAST(Op op, ASTPtr child)
      : unaryOp(op), childUnaryExp(std::move(child)), type(Type::UNARY_OP) {}

  bool isPrimary() const { return type == Type::PRIMARY; }
  bool isUnaryOp() const { return type == Type::UNARY_OP; }

private:
  enum class Type { PRIMARY, UNARY_OP } type;
};

template <typename Derived> class BinaryExpAST : public BaseAST {
public:
  ASTPtr singleExp;
  std::pair<ASTPtr, ASTPtr> compositeExp;
  Op binOp;

  BinaryExpAST(ASTPtr single)
      : singleExp(std::move(single)), type(Type::SINGLE) {}
  BinaryExpAST(ASTPtr lhs, Op oper, ASTPtr rhs)
      : compositeExp({std::move(lhs), std::move(rhs)}), binOp(oper),
        type(Type::COMPOSITE) {}
  bool isSingle() const { return type == Type::SINGLE; }
  bool isComposite() const { return type == Type::COMPOSITE; }

  void dump() const override;

protected:
  enum class Type { SINGLE, COMPOSITE } type;
  virtual std::string_view getASTNameImpl() const = 0;
};

// AddExp ::= MulExp | AddExp ("+" | "-") MulExp
class AddExpAST : public BinaryExpAST<AddExpAST> {
public:
  using BinaryExpAST<AddExpAST>::BinaryExpAST;

private:
  std::string_view getASTNameImpl() const override { return "AddExpAST"; }
};

// MulExp ::= UnaryExp | MulExp ("*" | "/" | "%") UnaryExp
class MulExpAST : public BinaryExpAST<MulExpAST> {
public:
  using BinaryExpAST<MulExpAST>::BinaryExpAST;

private:
  std::string_view getASTNameImpl() const override { return "MulExpAST"; }
};

template class BinaryExpAST<AddExpAST>;
template class BinaryExpAST<MulExpAST>;
} // namespace ast
#endif // __AST_AST_H__
