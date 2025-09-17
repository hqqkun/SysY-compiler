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
    return convertPrimaryExpr(
        builder, dynamic_cast<PrimaryExpAST *>(unaryExprAST->primaryExp.get()));
  } else if (unaryExprAST->isUnaryOp()) {
    ir::Value *childValue = convertUnaryExpr(
        builder,
        dynamic_cast<UnaryExpAST *>(unaryExprAST->childUnaryExp.get()));
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
      default: {
        assert(false && "Unknown unary operator");
        return nullptr;
      }
    }
  } else {
    assert(false && "Unknown unary expression type");
    return nullptr;
  }
}

ir::Value *IRGen::convertExpr(ir::IRBuilder &builder, ExprAST *exprAST) {
  assert(exprAST && "Expression AST cannot be null");
  return convertAddExpr(builder,
                        dynamic_cast<AddExpAST *>(exprAST->addExp.get()));
}

ir::Value *IRGen::convertAddExpr(ir::IRBuilder &builder,
                                 ast::AddExpAST *addExpAST) {
  assert(addExpAST && "Add expression AST cannot be null");
  if (addExpAST->isSingle()) {
    return convertMulExpr(
        builder, dynamic_cast<MulExpAST *>(addExpAST->singleExp.get()));
  } else if (addExpAST->isComposite()) {
    auto &p = addExpAST->compositeExp;
    ir::Value *lhs =
        convertAddExpr(builder, dynamic_cast<AddExpAST *>(p.first.get()));
    ir::Value *rhs =
        convertMulExpr(builder, dynamic_cast<MulExpAST *>(p.second.get()));
    switch (addExpAST->binOp) {
      case Op::PLUS:
        return builder.create<ir::AddOp>(lhs, rhs)->getResult();
      case Op::MINUS:
        return builder.create<ir::SubOp>(lhs, rhs)->getResult();
      default: {
        assert(false && "Unknown add operator");
        return nullptr;
      }
    }
  } else {
    assert(false && "Unknown add expression type");
    return nullptr;
  }
}

ir::Value *IRGen::convertMulExpr(ir::IRBuilder &builder,
                                 ast::MulExpAST *mulExpAST) {
  assert(mulExpAST && "Mul expression AST cannot be null");
  if (mulExpAST->isSingle()) {
    return convertUnaryExpr(
        builder, dynamic_cast<UnaryExpAST *>(mulExpAST->singleExp.get()));
  } else if (mulExpAST->isComposite()) {
    auto &p = mulExpAST->compositeExp;
    ir::Value *lhs =
        convertMulExpr(builder, dynamic_cast<MulExpAST *>(p.first.get()));
    ir::Value *rhs =
        convertUnaryExpr(builder, dynamic_cast<UnaryExpAST *>(p.second.get()));
    switch (mulExpAST->binOp) {
      case Op::MUL:
        return builder.create<ir::MulOp>(lhs, rhs)->getResult();
      case Op::DIV:
        return builder.create<ir::DivOp>(lhs, rhs)->getResult();
      case Op::MOD:
        return builder.create<ir::ModOp>(lhs, rhs)->getResult();
      default: {
        assert(false && "Unknown mul operator");
        return nullptr;
      }
    }
  } else {
    assert(false && "Unknown mul expression type");
    return nullptr;
  }
}

ir::Value *IRGen::convertPrimaryExpr(ir::IRBuilder &builder,
                                     PrimaryExpAST *primaryExpAST) {
  assert(primaryExpAST && "Primary expression AST cannot be null");
  if (primaryExpAST->isExp()) {
    return convertExpr(builder,
                       dynamic_cast<ExprAST *>(primaryExpAST->exp.get()));
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
  ir::Value *returnVal =
      convertExpr(builder, dynamic_cast<ExprAST *>(stmtAST->exp.get()));
  assert(returnVal && "Return value cannot be null");
  builder.create<ir::ReturnOp>(returnVal);
}

ir::BasicBlock *IRGen::convertBlock(BlockAST *blockAST) {
  if (!blockAST)
    return nullptr;

  ir::BasicBlock *block = ir::BasicBlock::create(context, "entry");
  ir::IRBuilder builder(context);
  builder.setInsertPoint(block);
  if (auto *stmt = dynamic_cast<StmtAST *>(blockAST->stmt.get())) {
    convertStmt(builder, stmt);
  }
  return block;
}

ir::Function *IRGen::generate(std::unique_ptr<ast::BaseAST> &ast) {
  auto *compUnit = dynamic_cast<CompUnitAST *>(ast.get());
  auto *funcDef =
      compUnit ? dynamic_cast<FuncDefAST *>(compUnit->funcDef.get()) : nullptr;
  auto *funcTypeAST =
      funcDef ? dynamic_cast<FuncTypeAST *>(funcDef->funcType.get()) : nullptr;
  auto funcType = funcTypeAST ? convertFunctionType(funcTypeAST) : nullptr;

  if (!funcDef || !funcType)
    return nullptr;

  ir::IRBuilder builder(context);
  ir::Function *function =
      ir::Function::create(context, funcDef->ident, funcType);
  if (auto *entryBlock =
          convertBlock(dynamic_cast<BlockAST *>(funcDef->block.get()))) {
    function->addBlock(entryBlock);
  }

  return function;
}

} // namespace conversion
