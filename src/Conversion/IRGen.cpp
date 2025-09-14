#include <cassert>

#include "AST/AST.h"
#include "AST/Ops.h"
#include "Conversion/IRGen.h"
#include "IR/Function.h"
#include "IR/IRBuilder.h"
#include "IR/Operation.h"

using namespace ast;

namespace conversion {

ir::Value *IRGen::convertUnaryExpr(ir::IRBuilder &builder,
                                   UnaryExpAST *unaryExprAST) {
  assert(unaryExprAST && "Unary expression AST cannot be null");
  if (unaryExprAST->isPrimary()) {
    PrimaryExpAST *primaryExpAST =
        dynamic_cast<PrimaryExpAST *>(unaryExprAST->primaryExp.get());
    assert(primaryExpAST && "Primary expression AST cannot be null");
    return convertPrimaryExpr(builder, primaryExpAST);
  } else if (unaryExprAST->isUnaryOp()) {
    UnaryExpAST *childUnaryExpAST =
        dynamic_cast<UnaryExpAST *>(unaryExprAST->childUnaryExp.get());
    assert(childUnaryExpAST && "Child unary expression AST cannot be null");
    ir::Value *childValue = convertUnaryExpr(builder, childUnaryExpAST);
    switch (unaryExprAST->unaryOp) {
      case Op::PLUS:
        return childValue; // Unary plus is a no-op.
      case Op::MINUS: {
        ir::Integer *zero = ir::Integer::get(builder.getContext(), 0, 32);
        return builder.create<ir::SubOp>(zero, childValue)->getResult();
      }
      case Op::BANG: {
        ir::Integer *zero = ir::Integer::get(builder.getContext(), 0, 32);
        return builder.create<ir::EqOp>(childValue, zero)->getResult();
      }
    }
  } else {
    assert(false && "Unknown unary expression type");
  }
  return nullptr;
}

ir::Value *IRGen::convertExpr(ir::IRBuilder &builder, ExprAST *exprAST) {
  assert(exprAST && "Expression AST cannot be null");
  UnaryExpAST *unaryExprAST =
      dynamic_cast<UnaryExpAST *>(exprAST->unaryExp.get());
  assert(unaryExprAST && "Only unary expressions are supported");
  return convertUnaryExpr(builder, unaryExprAST);
}

ir::Value *IRGen::convertPrimaryExpr(ir::IRBuilder &builder,
                                     PrimaryExpAST *primaryExpAST) {
  assert(primaryExpAST && "Primary expression AST cannot be null");
  if (primaryExpAST->isExp()) {
    ExprAST *innerExp = dynamic_cast<ExprAST *>(primaryExpAST->exp.get());
    assert(innerExp && "Inner expression AST cannot be null");
    return convertExpr(builder, innerExp);
  } else if (primaryExpAST->isNumber()) {
    return ir::Integer::get(builder.getContext(), primaryExpAST->number, 32);
  } else {
    assert(false && "Unknown primary expression type");
  }
}

ir::FunctionType *IRGen::convertFunctionType(FuncTypeAST *funcTypeAST) {
  if (!funcTypeAST || funcTypeAST->type != Type::INT)
    return nullptr;

  auto *intType = ir::IntegerType::get(context, 32);
  return ir::FunctionType::get(context, intType, {});
}

void IRGen::convertStmt(ir::IRBuilder &builder, StmtAST *stmtAST) {
  // Assume is a return stmt.
  assert(stmtAST && "Statement AST cannot be null");
  ExprAST *exprAST = dynamic_cast<ExprAST *>(stmtAST->exp.get());
  assert(exprAST && "Only return statements with expressions are supported");
  ir::Value *returnVal = convertExpr(builder, exprAST);
  builder.create<ir::ReturnOp>(returnVal);
}

ir::BasicBlock *IRGen::convertBlock(BlockAST *blockAST) {
  if (!blockAST)
    return nullptr;

  ir::BasicBlock *block = ir::BasicBlock::create(context, "entry");
  ir::IRBuilder builder(context);
  builder.setInsertPoint(block);
  if (auto stmt = dynamic_cast<StmtAST *>(blockAST->stmt.get())) {
    convertStmt(builder, stmt);
  }
  return block;
}

ir::Function *IRGen::generate(std::unique_ptr<ast::BaseAST> &ast) {
  CompUnitAST *compUnit = dynamic_cast<CompUnitAST *>(ast.get());
  FuncDefAST *funcDef =
      compUnit ? dynamic_cast<FuncDefAST *>(compUnit->funcDef.get()) : nullptr;
  FuncTypeAST *funcTypeAST =
      funcDef ? dynamic_cast<FuncTypeAST *>(funcDef->funcType.get()) : nullptr;
  auto funcType = funcTypeAST ? convertFunctionType(funcTypeAST) : nullptr;

  if (!funcDef || !funcType)
    return nullptr;

  ir::IRBuilder builder(context);
  ir::Function *function =
      ir::Function::create(context, funcDef->ident, funcType);
  if (ir::BasicBlock *entryBlock =
          convertBlock(dynamic_cast<BlockAST *>(funcDef->block.get()))) {
    function->addBlock(entryBlock);
  }

  return function;
}

} // namespace conversion
