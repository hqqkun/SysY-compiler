#ifndef __CONVERSION_IRGEN_H__
#define __CONVERSION_IRGEN_H__

#include "AST/AST.h"
#include "IR/BasicBlock.h"
#include "IR/Function.h"
#include "IR/IRBuilder.h"
#include "IR/IRContext.h"
#include "IR/Operation.h"
#include "IR/Type.h"
#include "IR/Value.h"

namespace conversion {

class IRGen {
public:
  explicit IRGen(ir::IRContext &ctx) : context(ctx) {}
  ir::Function *generate(std::unique_ptr<ast::BaseAST> &ast);

private:
  ir::IRContext &context;

  ir::BasicBlock *convertBlock(ast::BlockAST *blockAST);
  void convertStmt(ir::IRBuilder &builder, ast::StmtAST *stmtAST);
  ir::FunctionType *convertFunctionType(ast::FuncTypeAST *funcTypeAST);

  /// Convert an expression AST to an IR Value.
  ir::Value *convertExpr(ir::IRBuilder &builder, ast::ExprAST *exprAST);
  ir::Value *convertUnaryExpr(ir::IRBuilder &builder,
                              ast::UnaryExpAST *unaryExprAST);
  ir::Value *convertPrimaryExpr(ir::IRBuilder &builder,
                                ast::PrimaryExpAST *primaryExpAST);
  ir::Value *convertAddExpr(ir::IRBuilder &builder, ast::AddExpAST *addExpAST);
  ir::Value *convertMulExpr(ir::IRBuilder &builder, ast::MulExpAST *mulExpAST);
};

} // namespace conversion

#endif // __CONVERSION_IRGEN_H__
