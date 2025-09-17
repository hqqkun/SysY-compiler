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
  static ir::Value *convertMulExpr(ir::IRBuilder &builder,
                                   ast::MulExpAST *mulExpAST);
  static ir::Value *convertAddExpr(ir::IRBuilder &builder,
                                   ast::AddExpAST *addExpAST);
  static ir::Value *convertRelExpr(ir::IRBuilder &builder,
                                   ast::RelExpAST *relExpAST);
  static ir::Value *convertEqExpr(ir::IRBuilder &builder,
                                  ast::EqExpAST *eqExpAST);
  static ir::Value *convertLAndExpr(ir::IRBuilder &builder,
                                    ast::LAndExpAST *landExpAST);
  static ir::Value *convertLOrExpr(ir::IRBuilder &builder,
                                   ast::LOrExpAST *lorExpAST);

  template <typename DerivedAST, typename NextAST,
            ir::Value *(*NextConvertFunc)(ir::IRBuilder &, NextAST *)>
  static ir::Value *convertBinaryExp(ir::IRBuilder &builder, DerivedAST *ast,
                                     OpHandler op_handler);
};

} // namespace conversion

#endif // __CONVERSION_IRGEN_H__
