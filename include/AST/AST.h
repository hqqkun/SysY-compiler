#ifndef __AST_AST_H__
#define __AST_AST_H__

#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "AST/Ops.h"
#include "AST/Type.h"

namespace ast {
using ASTPtr = std::unique_ptr<class BaseAST>;
using BlockItemPtr = std::unique_ptr<class BlockItemAST>;

class ExprAST;

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

/// Decleration
class DeclAST : public BaseAST {
public:
  ASTPtr constDecl;
  ASTPtr varDecl;
  void dump() const override;

  DeclAST(ASTPtr decl);
  bool isConstDecl() const { return type == Type::CONST; }
  bool isVarDecl() const { return type == Type::VAR; }

private:
  enum class Type { CONST, VAR } type;
};

class ConstDeclAST : public BaseAST {
public:
  Type bType;
  std::unique_ptr<std::vector<ASTPtr>> constDefs;
  void dump() const override;

  ConstDeclAST(Type type, std::unique_ptr<std::vector<ASTPtr>> defs)
      : bType(type), constDefs(std::move(defs)) {}
};

class VarDeclAST : public BaseAST {
public:
  Type bType;
  std::unique_ptr<std::vector<ASTPtr>> varDefs;
  void dump() const override;

  VarDeclAST(Type type, std::unique_ptr<std::vector<ASTPtr>> defs)
      : bType(type), varDefs(std::move(defs)) {}
};

class ConstDefAST : public BaseAST {
public:
  std::string var;
  ASTPtr initVal;
  void dump() const override;

  ConstDefAST(const std::string &name, ASTPtr init)
      : var(name), initVal(std::move(init)) {}
};

class ConstInitValAST : public BaseAST {
public:
  ASTPtr constExp;
  void dump() const override;

  ConstInitValAST(ASTPtr exp) : constExp(std::move(exp)) {}
};

class VarDefAST : public BaseAST {
public:
  std::string var;
  ASTPtr initVal; // can be nullptr
  void dump() const override;

  VarDefAST(const std::string &name, ASTPtr init)
      : var(name), initVal(std::move(init)) {}
  VarDefAST(const std::string &name) : var(name), initVal(nullptr) {}
};

class InitValAST : public BaseAST {
public:
  ASTPtr exp;
  void dump() const override;

  InitValAST(ASTPtr e) : exp(std::move(e)) {}
};

class BlockAST : public BaseAST {
public:
  std::unique_ptr<std::vector<BlockItemPtr>> blockItems;
  void dump() const override;

  BlockAST(std::unique_ptr<std::vector<BlockItemPtr>> items)
      : blockItems(std::move(items)) {}
};

class BlockItemAST : public BaseAST {
public:
  ASTPtr decl;
  ASTPtr stmt;

  BlockItemAST(ASTPtr item);
  bool isDecl() const { return type == Type::DECL; }
  bool isStmt() const { return type == Type::STMT; }
  void dump() const override;

private:
  enum class Type { DECL, STMT } type;
};

/// Statement
class StmtAST : public BaseAST {};

class ReturnStmtAST : public StmtAST {
public:
  ASTPtr exp;
  void dump() const override;

  ReturnStmtAST() : exp(nullptr) {}
  ReturnStmtAST(ASTPtr e) : exp(std::move(e)) {}
};

class AssignStmtAST : public StmtAST {
public:
  ASTPtr lVal;
  ASTPtr exp;
  void dump() const override;

  AssignStmtAST(ASTPtr lval, ASTPtr e)
      : lVal(std::move(lval)), exp(std::move(e)) {}
};

class ExprStmtAST : public StmtAST {
public:
  ASTPtr exp; // can be nullptr.
  void dump() const override;

  ExprStmtAST() : exp(nullptr) {}
  ExprStmtAST(ASTPtr e) : exp(std::move(e)) {}
};

class BlockStmtAST : public StmtAST {
public:
  std::unique_ptr<BlockAST> block;
  void dump() const override;
  BlockStmtAST(std::unique_ptr<BlockAST> b) : block(std::move(b)) {}
};

class IfStmtAST : public StmtAST {
public:
  std::unique_ptr<ExprAST> cond;
  std::unique_ptr<StmtAST> thenStmt;
  std::unique_ptr<StmtAST> elseStmt; // can be nullptr
  void dump() const override;

  IfStmtAST(std::unique_ptr<ExprAST> c, std::unique_ptr<StmtAST> t)
      : cond(std::move(c)), thenStmt(std::move(t)), elseStmt(nullptr) {}
  IfStmtAST(std::unique_ptr<ExprAST> c, std::unique_ptr<StmtAST> t,
            std::unique_ptr<StmtAST> e)
      : cond(std::move(c)), thenStmt(std::move(t)), elseStmt(std::move(e)) {}
};

class WhileStmtAST : public StmtAST {
public:
  std::unique_ptr<ExprAST> cond;
  std::unique_ptr<StmtAST> body;
  void dump() const override;

  WhileStmtAST(std::unique_ptr<ExprAST> c, std::unique_ptr<StmtAST> b)
      : cond(std::move(c)), body(std::move(b)) {}
};

class BreakStmtAST : public StmtAST {
public:
  void dump() const override;
};

class ContinueStmtAST : public StmtAST {
public:
  void dump() const override;
};

/// Expression
class ExprAST : public BaseAST {
public:
  ASTPtr exp;
  void dump() const override;
};

class LValAST : public BaseAST {
public:
  std::string ident;
  void dump() const override;

  LValAST(const std::string &var) : ident(var) {}
};

// PrimaryExp ::= "(" Exp ")" | Number
class PrimaryExpAST : public BaseAST {
public:
  int number;
  ASTPtr exp;
  ASTPtr lVal;
  void dump() const override;

  PrimaryExpAST(int num) : number(num), type(Type::NUMBER) {}
  PrimaryExpAST(ASTPtr var) {
    if (dynamic_cast<ExprAST *>(var.get())) {
      exp = std::move(var);
      type = Type::EXP;
    } else if (dynamic_cast<LValAST *>(var.get())) {
      lVal = std::move(var);
      type = Type::LVAL;
    } else {
      assert(false && "var is neither exp or lval.");
    }
  }
  bool isNumber() const { return type == Type::NUMBER; }
  bool isExp() const { return type == Type::EXP; }
  bool isLVal() const { return type == Type::LVAL; }

private:
  enum class Type { NUMBER, EXP, LVAL } type;
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
class ConstExpAST : public BaseAST {
public:
  ASTPtr exp;
  void dump() const override;

  ConstExpAST(ASTPtr e) : exp(std::move(e)) {}
};

class BinaryExpAST : public BaseAST {
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
class AddExpAST : public BinaryExpAST {
public:
  using BinaryExpAST::BinaryExpAST;

private:
  std::string_view getASTNameImpl() const override { return "AddExpAST"; }
};

// MulExp ::= UnaryExp | MulExp ("*" | "/" | "%") UnaryExp
class MulExpAST : public BinaryExpAST {
public:
  using BinaryExpAST::BinaryExpAST;

private:
  std::string_view getASTNameImpl() const override { return "MulExpAST"; }
};

// RelExp ::= AddExp | RelExp ("<" | ">" | "<=" | ">=") AddExp
class RelExpAST : public BinaryExpAST {
public:
  using BinaryExpAST::BinaryExpAST;

private:
  std::string_view getASTNameImpl() const override { return "RelExpAST"; }
};

// EqExp ::= RelExp | EqExp ("==" | "!=") RelExp
class EqExpAST : public BinaryExpAST {
public:
  using BinaryExpAST::BinaryExpAST;

private:
  std::string_view getASTNameImpl() const override { return "EqExpAST"; }
};

// LAndExp ::= EqExp | LAndExp "&&" EqExp
class LAndExpAST : public BinaryExpAST {
public:
  using BinaryExpAST::BinaryExpAST;

private:
  std::string_view getASTNameImpl() const override { return "LAndExpAST"; }
};

// LOrExp ::= LAndExp | LOrExp "||" LAndExp
class LOrExpAST : public BinaryExpAST {
public:
  using BinaryExpAST::BinaryExpAST;

private:
  std::string_view getASTNameImpl() const override { return "LOrExpAST"; }
};

} // namespace ast
#endif // __AST_AST_H__
