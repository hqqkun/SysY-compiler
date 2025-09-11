#ifndef __AST_AST_H__
#define __AST_AST_H__

#include <memory>
#include <string>

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
  int number;
  void dump() const override;
};

} // namespace ast
#endif // __AST_AST_H__
