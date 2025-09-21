#include <cassert>

#include "AST/AST.h"
#include "AST/Ops.h"
#include "Conversion/IRGen.h"
#include "IR/Function.h"
#include "IR/IRBuilder.h"
#include "IR/Operation.h"

using namespace ast;

namespace conversion {

ir::Value *IRGen::dispatchAndConvert(ir::IRBuilder &builder,
                                     ast::BaseAST *ast) {
  assert(ast && "AST node cannot be null");
  if (auto *expr = dynamic_cast<ExprAST *>(ast)) {
    return convertExpr(builder, expr);
  } else if (auto *primary = dynamic_cast<PrimaryExpAST *>(ast)) {
    return convertPrimaryExpr(builder, primary);
  } else if (auto *unary = dynamic_cast<UnaryExpAST *>(ast)) {
    return convertUnaryExpr(builder, unary);
  } else if (auto *binary = dynamic_cast<BinaryExpAST *>(ast)) {
    return convertBinaryExp(builder, binary);
  } else {
    assert(false && "Unknown AST type");
  }
  return nullptr;
}

ir::Value *IRGen::convertExpr(ir::IRBuilder &builder, ExprAST *exprAST) {
  assert(exprAST && "Expression AST cannot be null");
  return dispatchAndConvert(builder, exprAST->exp.get());
}

ir::Value *IRGen::convertPrimaryExpr(ir::IRBuilder &builder,
                                     PrimaryExpAST *primaryExpAST) {
  assert(primaryExpAST && "Primary expression AST cannot be null");
  if (primaryExpAST->isExp()) {
    return dispatchAndConvert(builder, primaryExpAST->exp.get());
  } else if (primaryExpAST->isNumber()) {
    return ir::Integer::get(builder.getContext(), primaryExpAST->number, 32);
  } else {
    assert(false && "Unknown primary expression type");
  }
}

ir::Value *IRGen::convertUnaryExpr(ir::IRBuilder &builder,
                                   UnaryExpAST *unaryExprAST) {
  assert(unaryExprAST && "Unary expression AST cannot be null");
  if (unaryExprAST->isPrimary()) {
    return dispatchAndConvert(builder, unaryExprAST->primaryExp.get());
  } else if (unaryExprAST->isUnaryOp()) {
    ir::Value *childValue =
        dispatchAndConvert(builder, unaryExprAST->childUnaryExp.get());
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

ir::Value *IRGen::convertBinaryExp(ir::IRBuilder &builder,
                                   ast::BinaryExpAST *binaryExpAST) {
  if (binaryExpAST->isSingle()) {
    return dispatchAndConvert(builder, binaryExpAST->singleExp.get());
  } else if (binaryExpAST->isComposite()) {
    auto &[lhs_ast, rhs_ast] = binaryExpAST->compositeExp;
    ir::Value *lhs = dispatchAndConvert(builder, lhs_ast.get());
    ir::Value *rhs = dispatchAndConvert(builder, rhs_ast.get());
    switch (binaryExpAST->binOp) {
      case Op::PLUS:
        return builder.create<ir::AddOp>(lhs, rhs)->getResult();
      case Op::MINUS:
        return builder.create<ir::SubOp>(lhs, rhs)->getResult();
      case Op::MUL:
        return builder.create<ir::MulOp>(lhs, rhs)->getResult();
      case Op::DIV:
        return builder.create<ir::DivOp>(lhs, rhs)->getResult();
      case Op::MOD:
        return builder.create<ir::ModOp>(lhs, rhs)->getResult();
      case Op::LT:
        return builder.create<ir::LessOp>(lhs, rhs)->getResult();
      case Op::GT:
        return builder.create<ir::GreaterOp>(lhs, rhs)->getResult();
      case Op::LE:
        return builder.create<ir::LessEqualOp>(lhs, rhs)->getResult();
      case Op::GE:
        return builder.create<ir::GreaterEqualOp>(lhs, rhs)->getResult();
      case Op::EQ:
        return builder.create<ir::EqOp>(lhs, rhs)->getResult();
      case Op::NEQ:
        return builder.create<ir::NeqOp>(lhs, rhs)->getResult();
      case Op::LAND: {
        ir::Integer *zero = ir::Integer::get(builder.getContext(), 0, 32);
        ir::Value *lhsBool = builder.create<ir::NeqOp>(lhs, zero)->getResult();
        ir::Value *rhsBool = builder.create<ir::NeqOp>(rhs, zero)->getResult();
        return builder.create<ir::BitAndOp>(lhsBool, rhsBool)->getResult();
      }
      case Op::LOR: {
        ir::Integer *zero = ir::Integer::get(builder.getContext(), 0, 32);
        ir::Value *lhsBool = builder.create<ir::NeqOp>(lhs, zero)->getResult();
        ir::Value *rhsBool = builder.create<ir::NeqOp>(rhs, zero)->getResult();
        return builder.create<ir::BitOrOp>(lhsBool, rhsBool)->getResult();
      }
      default:
        assert(false && "Unknown binary operator");
        return nullptr;
    }
  } else {
    assert(false && "Unknown expression type");
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
  ir::Value *returnVal = dispatchAndConvert(builder, stmtAST->exp.get());
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
