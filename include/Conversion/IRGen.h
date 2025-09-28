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
  explicit IRGen(ir::IRContext &ctx)
      : context(ctx), interpreter(varTables), nextBlockId(0), nextTempId(0) {}
  ir::Function *generate(std::unique_ptr<ast::BaseAST> &ast);

private:
  ir::IRContext &context;
  SymbolTable varTables;
  interpreter::Interpreter interpreter;
  uint64_t nextBlockId;
  uint64_t nextTempId;

  void convertBlock(ir::IRBuilder &builder, ast::BlockAST *blockAST);

  /// Convert statements.
  void convertStmt(ir::IRBuilder &builder, ast::StmtAST *stmtAST);
  void convertReturnStmt(ir::IRBuilder &builder,
                         ast::ReturnStmtAST *returnStmtAST);
  void convertAssignStmt(ir::IRBuilder &builder,
                         ast::AssignStmtAST *assignStmtAST);
  void convertIfStmt(ir::IRBuilder &builder, ast::IfStmtAST *ifStmtAST);
  void convertWhileStmt(ir::IRBuilder &builder,
                        ast::WhileStmtAST *whileStmtAST);
  ir::FunctionType *convertFunctionType(ast::FuncTypeAST *funcTypeAST);

  /// Convert declarations and definitions.
  void convertDeclaration(ir::IRBuilder &builder, ast::DeclAST *declAST);
  void convertConstDecl(ast::ConstDeclAST *constDeclAST);
  void convertVarDecl(ir::IRBuilder &builder, ast::VarDeclAST *varDeclAST);
  void convertConstDef(ast::ConstDefAST *constDefAST);
  void convertVarDef(ir::IRBuilder &builder, ast::VarDefAST *varDefAST,
                     ast::Type bType = ast::Type::INT);

  /// Convert an expression AST to an IR Value.
  ir::Value *convertExpr(ir::IRBuilder &builder, ast::ExprAST *exprAST);
  ir::Value *convertPrimaryExpr(ir::IRBuilder &builder,
                                ast::PrimaryExpAST *primaryExpAST);
  ir::Value *convertUnaryExpr(ir::IRBuilder &builder,
                              ast::UnaryExpAST *unaryExprAST);
  ir::Value *convertBinaryExp(ir::IRBuilder &builder,
                              ast::BinaryExpAST *binaryExpAST);
  ir::Value *convertLogicalBinaryExp(ir::IRBuilder &builder,
                                     ast::BinaryExpAST *binaryExpAST);
  ir::Value *convertInitValExp(ir::IRBuilder &builder,
                               ast::InitValAST *initValAST);
  ir::Value *dispatchAndConvert(ir::IRBuilder &builder, ast::BaseAST *ast);

  ir::Value *convertLval(ir::IRBuilder &builder, ast::LValAST *lvalAST);

  /// Utility functions for block id management.
  uint64_t getNextBlockId() { return nextBlockId++; }
  uint64_t getNextTempId() { return nextTempId++; }
  void resetBlockId() { nextBlockId = 0; }
  void resetTempId() { nextTempId = 0; }
};

} // namespace conversion

#endif // __CONVERSION_IRGEN_H__
