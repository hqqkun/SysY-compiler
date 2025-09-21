#ifndef __INTERPRETER_INTERPRETER_H__
#define __INTERPRETER_INTERPRETER_H__

#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

#include "AST/AST.h"
#include "Utils/Utils.h"

namespace interpreter {

class Interpreter {
public:
  explicit Interpreter(SymbolTable &table) : varTables(table) {}
  int32_t evalExpr(ast::ExprAST *expr) const;

private:
  SymbolTable &varTables;

  int32_t dispatchAndEval(ast::BaseAST *ast) const;
  int32_t evalPrimaryExp(ast::PrimaryExpAST *primaryExp) const;
  int32_t evalUnaryExp(ast::UnaryExpAST *unaryExp) const;
  int32_t evalLVal(ast::LValAST *lval) const;
  int32_t evalConstExp(ast::ConstExpAST *constExp) const;
  int32_t evalBinaryExp(ast::BinaryExpAST *binaryExp) const;
};

} // namespace interpreter

#endif // __INTERPRETER_INTERPRETER_H__
