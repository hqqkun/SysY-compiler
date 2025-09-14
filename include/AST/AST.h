#ifndef __AST_AST_H__
#define __AST_AST_H__

#include <memory>
#include <string>

#include "AST/Ops.h"
#include "AST/Type.h"

namespace ast {
class BaseAST {
public:
  virtual ~BaseAST() = default;
  virtual void dump() const = 0;
};

class CompUnitAST : public BaseAST {
public:
  std::unique_ptr<BaseAST> funcDef;
  void dump() const override;
};

class FuncDefAST : public BaseAST {
public:
  std::unique_ptr<BaseAST> funcType;
  std::string ident;
  std::unique_ptr<BaseAST> block;
  void dump() const override;
};

class FuncTypeAST : public BaseAST {
public:
  Type type;
  void dump() const override;
};

class BlockAST : public BaseAST {
public:
  std::unique_ptr<BaseAST> stmt;
  void dump() const override;
};

class StmtAST : public BaseAST {
public:
  std::unique_ptr<BaseAST> exp;
  void dump() const override;
};

class ExprAST : public BaseAST {
public:
  std::unique_ptr<BaseAST> unaryExp;
  void dump() const override;
};

// PrimaryExp ::= "(" Exp ")" | Number
class PrimaryExpAST : public BaseAST {
public:
  int number;
  std::unique_ptr<BaseAST> exp;
  void dump() const override;

  PrimaryExpAST(int num) : number(num), type(Type::NUMBER) {}
  PrimaryExpAST(std::unique_ptr<BaseAST> e)
      : exp(std::move(e)), type(Type::EXP) {}
  bool isNumber() const { return type == Type::NUMBER; }
  bool isExp() const { return type == Type::EXP; }

private:
  enum class Type { NUMBER, EXP } type;
};

// UnaryExp ::= PrimaryExp | UnaryOp UnaryExp
class UnaryExpAST : public BaseAST {
public:
  std::unique_ptr<BaseAST> primaryExp;
  Op unaryOp;
  std::unique_ptr<BaseAST> childUnaryExp;
  void dump() const override;

  UnaryExpAST(std::unique_ptr<BaseAST> primary)
      : primaryExp(std::move(primary)), type(Type::PRIMARY) {}

  UnaryExpAST(Op op, std::unique_ptr<BaseAST> child)
      : unaryOp(op), childUnaryExp(std::move(child)), type(Type::UNARY_OP) {}

  bool isPrimary() const { return type == Type::PRIMARY; }
  bool isUnaryOp() const { return type == Type::UNARY_OP; }

private:
  enum class Type { PRIMARY, UNARY_OP } type;
};

} // namespace ast
#endif // __AST_AST_H__
