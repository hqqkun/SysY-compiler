#ifndef __CONVERSION_IRGEN_H__
#define __CONVERSION_IRGEN_H__

#include <functional>

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
  using OpHandler = std::function<ir::Value *(ir::IRBuilder &, ast::Op,
                                              ir::Value *, ir::Value *)>;

  ir::IRContext &context;

  ir::BasicBlock *convertBlock(ast::BlockAST *blockAST);
  void convertStmt(ir::IRBuilder &builder, ast::StmtAST *stmtAST);
  ir::FunctionType *convertFunctionType(ast::FuncTypeAST *funcTypeAST);

  /// Convert an expression AST to an IR Value.
  static ir::Value *convertExpr(ir::IRBuilder &builder, ast::ExprAST *exprAST);
  static ir::Value *convertPrimaryExpr(ir::IRBuilder &builder,
                                       ast::PrimaryExpAST *primaryExpAST);
  static ir::Value *convertUnaryExpr(ir::IRBuilder &builder,
                                     ast::UnaryExpAST *unaryExprAST);
  static ir::Value *convertBinaryExp(ir::IRBuilder &builder,
                                     ast::BinaryExpAST *binaryExpAST);
  static ir::Value *dispatchAndConvert(ir::IRBuilder &builder,
                                       ast::BaseAST *ast);
};

} // namespace conversion

#endif // __CONVERSION_IRGEN_H__
