#ifndef __CONVERSION_IRGEN_H__
#define __CONVERSION_IRGEN_H__

#include "AST/AST.h"
#include "IR/BasicBlock.h"
#include "IR/Function.h"
#include "IR/IRBuilder.h"
#include "IR/IRContext.h"
#include "IR/Module.h"
#include "IR/Operation.h"
#include "IR/Type.h"
#include "IR/Value.h"
#include "Interpreter/Interpreter.h"
#include "Utils/Utils.h"

namespace conversion {

/// A pair representing the loop's (entry block, exit block).
using LoopPair = std::pair<ir::BasicBlock *, ir::BasicBlock *>;
/// A stack to manage nested loops.
using LoopStack = std::list<LoopPair>;

class IRGen {
public:
  explicit IRGen(ir::IRContext &ctx)
      : context(ctx), interpreter(varTables), nextBlockId(0), nextTempId(0) {}
  ir::Module *generate(std::unique_ptr<ast::BaseAST> &ast);

private:
  ir::IRContext &context;
  SymbolTable varTables;
  interpreter::Interpreter interpreter;
  uint64_t nextBlockId;
  uint64_t nextTempId;
  LoopStack loopStack;

  /// Add library function declarations to the symbol table.
  void addLibFuncDeclarations(ir::Module *module);

  /// Convert function definition.
  ir::Function *convertFuncDef(ast::FuncDefAST *funcDefAST);
  ir::FunctionType *
  convertFunctionType(const ast::Type &retType,
                      const std::vector<const ast::Type *> &paramTypes);
  void prepareFunctionArgs(ir::IRBuilder &builder, ir::Function *function);

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
  void convertBreakStmt(ir::IRBuilder &builder,
                        ast::BreakStmtAST *breakStmtAST);
  void convertContinueStmt(ir::IRBuilder &builder,
                           ast::ContinueStmtAST *continueStmtAST);

  /// Convert declarations and definitions.
  void convertDeclaration(ir::IRBuilder &builder, ast::DeclAST *declAST);
  void convertConstDecl(ast::ConstDeclAST *constDeclAST);
  void convertVarDecl(ir::IRBuilder &builder, ast::VarDeclAST *varDeclAST);
  void convertConstDef(ast::ConstDefAST *constDefAST);
  void convertVarDef(ir::IRBuilder &builder, ast::VarDefAST *varDefAST,
                     const ast::Type &bType);

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
  ir::Value *convertLval(ir::IRBuilder &builder, ast::LValAST *lvalAST);
  ir::Value *convertFuncCall(ir::IRBuilder &builder,
                             ast::FuncCallAST *funcCallAST);
  ir::Value *dispatchAndConvert(ir::IRBuilder &builder, ast::BaseAST *ast);

  /// Create an unreachable block to prevent fall-through after a jump or
  /// return.
  void createUnreachableBlock(ir::IRBuilder &builder);

  /// Utility functions for block id management.
  uint64_t getNextBlockId() { return nextBlockId++; }
  uint64_t getNextTempId() { return nextTempId++; }
  void resetBlockId() { nextBlockId = 0; }
  void resetTempId() { nextTempId = 0; }
};

} // namespace conversion

#endif // __CONVERSION_IRGEN_H__
