#include <cassert>

#include "AST/AST.h"
#include "AST/Ops.h"
#include "Conversion/IRGen.h"
#include "IR/Function.h"
#include "IR/IRBuilder.h"
#include "IR/Operation.h"

using namespace ast;

namespace conversion {

ir::Value *IRGen::convertExpr(ir::IRBuilder &builder, ExprAST *exprAST) {
  assert(exprAST && "Expression AST cannot be null");
  return convertLOrExpr(builder, dynamic_cast<LOrExpAST *>(exprAST->exp.get()));
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

template <typename DerivedAST, typename NextAST,
          ir::Value *(NextConvertFunc)(ir::IRBuilder &, NextAST *)>
ir::Value *IRGen::convertBinaryExp(ir::IRBuilder &builder, DerivedAST *ast,
                                   OpHandler op_handler) {
  assert(ast && "Expression AST cannot be null");

  if (ast->isSingle()) {
    NextAST *next_ast = dynamic_cast<NextAST *>(ast->singleExp.get());
    assert(next_ast && "Mismatched next-level AST type");
    return NextConvertFunc(builder, next_ast);
  } else if (ast->isComposite()) {
    auto &[lhs_ast, rhs_ast] = ast->compositeExp;

    DerivedAST *lhs_derived = dynamic_cast<DerivedAST *>(lhs_ast.get());
    assert(lhs_derived && "Mismatched LHS AST type");
    ir::Value *lhs = convertBinaryExp<DerivedAST, NextAST, NextConvertFunc>(
        builder, lhs_derived, op_handler);

    NextAST *rhs_next = dynamic_cast<NextAST *>(rhs_ast.get());
    assert(rhs_next && "Mismatched RHS AST type");
    ir::Value *rhs = NextConvertFunc(builder, rhs_next);

    // Callback for operation handling.
    return op_handler(builder, ast->binOp, lhs, rhs);
  } else {
    assert(false && "Unknown expression type");
    return nullptr;
  }
}

ir::Value *IRGen::convertMulExpr(ir::IRBuilder &builder,
                                 ast::MulExpAST *mulExpAST) {
  return convertBinaryExp<ast::MulExpAST, ast::UnaryExpAST,
                          IRGen::convertUnaryExpr>(
      builder, mulExpAST,
      [](ir::IRBuilder &b, Op op, ir::Value *lhs,
         ir::Value *rhs) -> ir::Value * {
        switch (op) {
          case Op::MUL:
            return b.create<ir::MulOp>(lhs, rhs)->getResult();
          case Op::DIV:
            return b.create<ir::DivOp>(lhs, rhs)->getResult();
          case Op::MOD:
            return b.create<ir::ModOp>(lhs, rhs)->getResult();
          default:
            assert(false && "Unknown mul operator");
            return nullptr;
        }
      });
}

ir::Value *IRGen::convertAddExpr(ir::IRBuilder &builder,
                                 ast::AddExpAST *addExpAST) {
  return convertBinaryExp<ast::AddExpAST, ast::MulExpAST,
                          IRGen::convertMulExpr>(
      builder, addExpAST,
      [](ir::IRBuilder &b, Op op, ir::Value *lhs,
         ir::Value *rhs) -> ir::Value * {
        switch (op) {
          case Op::PLUS:
            return b.create<ir::AddOp>(lhs, rhs)->getResult();
          case Op::MINUS:
            return b.create<ir::SubOp>(lhs, rhs)->getResult();
          default:
            assert(false && "Unknown add operator");
            return nullptr;
        }
      });
}

ir::Value *IRGen::convertRelExpr(ir::IRBuilder &builder,
                                 ast::RelExpAST *relExpAST) {
  return convertBinaryExp<ast::RelExpAST, ast::AddExpAST,
                          IRGen::convertAddExpr>(
      builder, relExpAST,
      [](ir::IRBuilder &b, Op op, ir::Value *lhs,
         ir::Value *rhs) -> ir::Value * {
        switch (op) {
          case Op::LT:
            return b.create<ir::LessOp>(lhs, rhs)->getResult();
          case Op::GT:
            return b.create<ir::GreaterOp>(lhs, rhs)->getResult();
          case Op::LE:
            return b.create<ir::LessEqualOp>(lhs, rhs)->getResult();
          case Op::GE:
            return b.create<ir::GreaterEqualOp>(lhs, rhs)->getResult();
          default:
            assert(false && "Unknown relational operator");
            return nullptr;
        }
      });
}

ir::Value *IRGen::convertEqExpr(ir::IRBuilder &builder,
                                ast::EqExpAST *eqExpAST) {
  return convertBinaryExp<ast::EqExpAST, ast::RelExpAST, IRGen::convertRelExpr>(
      builder, eqExpAST,
      [](ir::IRBuilder &b, Op op, ir::Value *lhs,
         ir::Value *rhs) -> ir::Value * {
        switch (op) {
          case Op::EQ:
            return b.create<ir::EqOp>(lhs, rhs)->getResult();
          case Op::NEQ:
            return b.create<ir::NeqOp>(lhs, rhs)->getResult();
          default:
            assert(false && "Unknown equality operator");
            return nullptr;
        }
      });
}

ir::Value *IRGen::convertLAndExpr(ir::IRBuilder &builder,
                                  ast::LAndExpAST *landExpAST) {
  return convertBinaryExp<ast::LAndExpAST, ast::EqExpAST, IRGen::convertEqExpr>(
      builder, landExpAST,
      [](ir::IRBuilder &b, Op op, ir::Value *lhs,
         ir::Value *rhs) -> ir::Value * {
        if (op != Op::LAND) {
          assert(false && "Unknown logical AND operator");
          return nullptr;
        }
        ir::Value *zero = ir::Integer::get(b.getContext(), 0);
        ir::Value *lhsBool = b.create<ir::NeqOp>(lhs, zero)->getResult();
        ir::Value *rhsBool = b.create<ir::NeqOp>(rhs, zero)->getResult();
        return b.create<ir::BitAndOp>(lhsBool, rhsBool)->getResult();
      });
}

ir::Value *IRGen::convertLOrExpr(ir::IRBuilder &builder,
                                 ast::LOrExpAST *lorExpAST) {
  return convertBinaryExp<ast::LOrExpAST, ast::LAndExpAST,
                          IRGen::convertLAndExpr>(
      builder, lorExpAST,
      [](ir::IRBuilder &b, Op op, ir::Value *lhs,
         ir::Value *rhs) -> ir::Value * {
        if (op != Op::LOR) {
          assert(false && "Unknown logical OR operator");
          return nullptr;
        }
        ir::Value *zero = ir::Integer::get(b.getContext(), 0);
        ir::Value *lhsBool = b.create<ir::NeqOp>(lhs, zero)->getResult();
        ir::Value *rhsBool = b.create<ir::NeqOp>(rhs, zero)->getResult();
        return b.create<ir::BitOrOp>(lhsBool, rhsBool)->getResult();
      });
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
