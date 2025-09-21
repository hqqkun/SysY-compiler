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
#include "Interpreter/Interpreter.h"
#include "Utils/Utils.h"

namespace conversion {

class IRGen {
public:
  explicit IRGen(ir::IRContext &ctx) : context(ctx), interpreter(varTables) {}
  ir::Function *generate(std::unique_ptr<ast::BaseAST> &ast);

private:
  ir::IRContext &context;
  SymbolTable varTables;
  interpreter::Interpreter interpreter;

  ir::BasicBlock *convertBlock(ast::BlockAST *blockAST);
  void convertStmt(ir::IRBuilder &builder, ast::StmtAST *stmtAST);
  ir::FunctionType *convertFunctionType(ast::FuncTypeAST *funcTypeAST);

  /// Convert declarations and definitions.
  void convertDeclaration(ast::DeclAST *declAST);
  void convertConstDecl(ast::ConstDeclAST *constDeclAST);
  void convertConstDef(ast::ConstDefAST *constDefAST);

  /// Convert an expression AST to an IR Value.
  ir::Value *convertExpr(ir::IRBuilder &builder, ast::ExprAST *exprAST);
  ir::Value *convertPrimaryExpr(ir::IRBuilder &builder,
                                ast::PrimaryExpAST *primaryExpAST);
  ir::Value *convertUnaryExpr(ir::IRBuilder &builder,
                              ast::UnaryExpAST *unaryExprAST);
  ir::Value *convertBinaryExp(ir::IRBuilder &builder,
                              ast::BinaryExpAST *binaryExpAST);
  ir::Value *dispatchAndConvert(ir::IRBuilder &builder, ast::BaseAST *ast);
  int32_t convertLval(ast::LValAST *lvalAST);
};

} // namespace conversion

#endif // __CONVERSION_IRGEN_H__
