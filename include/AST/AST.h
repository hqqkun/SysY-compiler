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
using FuncFParamPtr = std::unique_ptr<class FuncFParamAST>;
using FuncRParamPtr = std::unique_ptr<class ExprAST>;
using FuncCallPtr = std::unique_ptr<class FuncCallAST>;
using TypePtr = std::unique_ptr<struct Type>;
using ConstExpPtr = std::unique_ptr<class ConstExpAST>;
using ConstInitValASTPtr = std::unique_ptr<class ConstInitValAST>;
using InitValASTPtr = std::unique_ptr<class InitValAST>;
using ExprPtr = std::unique_ptr<class ExprAST>;

class BaseAST {
public:
  virtual ~BaseAST() = default;
};

class CompUnitAST : public BaseAST {
public:
  std::unique_ptr<std::vector<ASTPtr>> topLevelNodes;

  CompUnitAST(std::unique_ptr<std::vector<ASTPtr>> nodes)
      : topLevelNodes(std::move(nodes)) {}
};

class FuncDefAST : public BaseAST {
public:
  TypePtr retType;
  std::string ident;
  std::unique_ptr<std::vector<FuncFParamPtr>> funcFParams;
  ASTPtr block;

  FuncDefAST(TypePtr type, const std::string &name, ASTPtr b)
      : retType(std::move(type)), ident(name), funcFParams(nullptr),
        block(std::move(b)) {}
  FuncDefAST(TypePtr type, const std::string &name,
             std::unique_ptr<std::vector<FuncFParamPtr>> params, ASTPtr b)
      : retType(std::move(type)), ident(name), funcFParams(std::move(params)),
        block(std::move(b)) {}

  bool hasParams() const { return funcFParams != nullptr; }
  const Type *getReturnType() const;
  const Type *getParamType(size_t index) const;
  const std::vector<const Type *> getParamTypes() const;
  std::vector<std::string> getParamNames() const;
};

/// Function formal parameter
class FuncFParamAST : public BaseAST {
public:
  TypePtr type;
  std::string ident;

  FuncFParamAST(TypePtr t, const std::string &name)
      : type(std::move(t)), ident(name) {}
};

/// Decleration
class DeclAST : public BaseAST {
public:
  ASTPtr constDecl;
  ASTPtr varDecl;

  DeclAST(ASTPtr decl);
  bool isConstDecl() const { return type == Type::CONST; }
  bool isVarDecl() const { return type == Type::VAR; }

private:
  enum class Type { CONST, VAR } type;
};

class ConstDeclAST : public BaseAST {
public:
  TypePtr bType;
  std::unique_ptr<std::vector<ASTPtr>> constDefs;

  ConstDeclAST(TypePtr type, std::unique_ptr<std::vector<ASTPtr>> defs)
      : bType(std::move(type)), constDefs(std::move(defs)) {}
};

class VarDeclAST : public BaseAST {
public:
  TypePtr bType;
  std::unique_ptr<std::vector<ASTPtr>> varDefs;

  VarDeclAST(TypePtr type, std::unique_ptr<std::vector<ASTPtr>> defs)
      : bType(std::move(type)), varDefs(std::move(defs)) {}
};

class ConstDefAST : public BaseAST {
public:
  std::string var;
  std::unique_ptr<std::vector<ConstExpPtr>> arraySizes;
  ConstInitValASTPtr initVal;

  bool isScalar() const { return type == Type::SCALAR; }
  bool isArray() const { return type == Type::ARRAY; }

  ConstDefAST(const std::string &name, ConstInitValASTPtr init)
      : var(name), arraySizes(nullptr), initVal(std::move(init)),
        type(Type::SCALAR) {}
  ConstDefAST(const std::string &name,
              std::unique_ptr<std::vector<ConstExpPtr>> sizes,
              ConstInitValASTPtr init)
      : var(name), arraySizes(std::move(sizes)), initVal(std::move(init)),
        type(Type::ARRAY) {}

private:
  enum class Type { SCALAR, ARRAY } type;
};

class ConstInitValAST : public BaseAST {
public:
  ConstExpPtr constExp;
  std::unique_ptr<std::vector<ConstInitValASTPtr>> constInitVec;
  bool isScalar() const { return type == Type::SCALAR; }
  bool isArray() const { return type == Type::ARRAY; }

  ConstInitValAST(ConstExpPtr exp)
      : constExp(std::move(exp)), constInitVec(nullptr), type(Type::SCALAR) {}
  ConstInitValAST(std::unique_ptr<std::vector<ConstInitValASTPtr>> inits)
      : constExp(nullptr), constInitVec(std::move(inits)), type(Type::ARRAY) {}

private:
  enum class Type { SCALAR, ARRAY } type;
};

class VarDefAST : public BaseAST {
public:
  std::string var;
  InitValASTPtr initVal;
  std::unique_ptr<std::vector<ConstExpPtr>> arraySizes;
  bool isScalar() const { return type == Type::SCALAR; }
  bool isArray() const { return type == Type::ARRAY; }

  VarDefAST(const std::string &name, InitValASTPtr init)
      : var(name), initVal(std::move(init)), arraySizes(nullptr),
        type(Type::SCALAR) {}
  VarDefAST(const std::string &name)
      : var(name), initVal(nullptr), arraySizes(nullptr), type(Type::SCALAR) {}
  VarDefAST(const std::string &name,
            std::unique_ptr<std::vector<ConstExpPtr>> sizes)
      : var(name), initVal(nullptr), arraySizes(std::move(sizes)),
        type(Type::ARRAY) {}
  VarDefAST(const std::string &name,
            std::unique_ptr<std::vector<ConstExpPtr>> sizes, InitValASTPtr init)
      : var(name), initVal(std::move(init)), arraySizes(std::move(sizes)),
        type(Type::ARRAY) {}

private:
  enum class Type { SCALAR, ARRAY } type;
};

class InitValAST : public BaseAST {
public:
  ExprPtr exp;
  std::unique_ptr<std::vector<InitValASTPtr>> initValVec;
  bool isScalar() const { return type == Type::SCALAR; }
  bool isArray() const { return type == Type::ARRAY; }

  InitValAST(ExprPtr e)
      : exp(std::move(e)), initValVec(nullptr), type(Type::SCALAR) {}
  InitValAST(std::unique_ptr<std::vector<InitValASTPtr>> exps)
      : exp(nullptr), initValVec(std::move(exps)), type(Type::ARRAY) {}

private:
  enum class Type { SCALAR, ARRAY } type;
};

class BlockAST : public BaseAST {
public:
  std::unique_ptr<std::vector<BlockItemPtr>> blockItems;

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

private:
  enum class Type { DECL, STMT } type;
};

/// Statement
class StmtAST : public BaseAST {};

class ReturnStmtAST : public StmtAST {
public:
  ASTPtr exp;

  ReturnStmtAST() : exp(nullptr) {}
  ReturnStmtAST(ASTPtr e) : exp(std::move(e)) {}
};

class AssignStmtAST : public StmtAST {
public:
  ASTPtr lVal;
  ASTPtr exp;

  AssignStmtAST(ASTPtr lval, ASTPtr e)
      : lVal(std::move(lval)), exp(std::move(e)) {}
};

class ExprStmtAST : public StmtAST {
public:
  ASTPtr exp; // can be nullptr.

  ExprStmtAST() : exp(nullptr) {}
  ExprStmtAST(ASTPtr e) : exp(std::move(e)) {}
};

class BlockStmtAST : public StmtAST {
public:
  std::unique_ptr<BlockAST> block;

  BlockStmtAST(std::unique_ptr<BlockAST> b) : block(std::move(b)) {}
};

class IfStmtAST : public StmtAST {
public:
  ExprPtr cond;
  std::unique_ptr<StmtAST> thenStmt;
  std::unique_ptr<StmtAST> elseStmt; // can be nullptr

  IfStmtAST(ExprPtr c, std::unique_ptr<StmtAST> t)
      : cond(std::move(c)), thenStmt(std::move(t)), elseStmt(nullptr) {}
  IfStmtAST(ExprPtr c, std::unique_ptr<StmtAST> t, std::unique_ptr<StmtAST> e)
      : cond(std::move(c)), thenStmt(std::move(t)), elseStmt(std::move(e)) {}
};

class WhileStmtAST : public StmtAST {
public:
  ExprPtr cond;
  std::unique_ptr<StmtAST> body;

  WhileStmtAST(ExprPtr c, std::unique_ptr<StmtAST> b)
      : cond(std::move(c)), body(std::move(b)) {}
};

class BreakStmtAST : public StmtAST {
public:
};

class ContinueStmtAST : public StmtAST {
public:
};

/// Expression
class ExprAST : public BaseAST {
public:
  ASTPtr exp;
};

class LValAST : public BaseAST {
public:
  std::string ident;
  std::unique_ptr<std::vector<ExprPtr>> arrayIndices;
  bool isScalar() const { return type == Type::SCALAR; }
  bool isArray() const { return type == Type::ARRAY; }

  LValAST(const std::string &var)
      : ident(var), arrayIndices(nullptr), type(Type::SCALAR) {}
  LValAST(const std::string &var, std::unique_ptr<std::vector<ExprPtr>> index)
      : ident(var), arrayIndices(std::move(index)), type(Type::ARRAY) {}

private:
  enum class Type { SCALAR, ARRAY } type;
};

// PrimaryExp ::= "(" Exp ")" | Number
class PrimaryExpAST : public BaseAST {
public:
  int number;
  ASTPtr exp;
  ASTPtr lVal;

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

// UnaryExp ::= PrimaryExp | UnaryOp UnaryExp | FuncCall
class UnaryExpAST : public BaseAST {
public:
  ASTPtr primaryExp;
  Op unaryOp;
  ASTPtr childUnaryExp;
  FuncCallPtr funcCall;

  UnaryExpAST(ASTPtr primary)
      : primaryExp(std::move(primary)), type(Type::PRIMARY) {}

  UnaryExpAST(Op op, ASTPtr child)
      : unaryOp(op), childUnaryExp(std::move(child)), type(Type::UNARY_OP) {}

  UnaryExpAST(FuncCallPtr call)
      : funcCall(std::move(call)), type(Type::FCALL) {}

  bool isPrimary() const { return type == Type::PRIMARY; }
  bool isUnaryOp() const { return type == Type::UNARY_OP; }
  bool isFuncCall() const { return type == Type::FCALL; }

private:
  enum class Type { PRIMARY, UNARY_OP, FCALL } type;
};

class FuncCallAST : public BaseAST {
public:
  std::string ident;
  std::unique_ptr<std::vector<FuncRParamPtr>> funcRParams;

  FuncCallAST(const std::string &name) : ident(name), funcRParams(nullptr) {}
  FuncCallAST(const std::string &name,
              std::unique_ptr<std::vector<FuncRParamPtr>> params)
      : ident(name), funcRParams(std::move(params)) {}

  bool hasParams() const { return funcRParams != nullptr; }
};

class ConstExpAST : public BaseAST {
public:
  ASTPtr exp;

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
