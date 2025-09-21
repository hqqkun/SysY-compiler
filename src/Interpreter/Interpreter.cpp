#include <cassert>
#include <cstdint>
#include <stdexcept>

#include "AST/AST.h"
#include "Interpreter/Interpreter.h"

namespace interpreter {

int32_t Interpreter::dispatchAndEval(ast::BaseAST *ast) const {
  assert(ast && "AST is null");
  if (auto expr = dynamic_cast<ast::ExprAST *>(ast)) {
    return evalExpr(expr);
  } else if (auto primary = dynamic_cast<ast::PrimaryExpAST *>(ast)) {
    return evalPrimaryExp(primary);
  } else if (auto unary = dynamic_cast<ast::UnaryExpAST *>(ast)) {
    return evalUnaryExp(unary);
  } else if (auto binary = dynamic_cast<ast::BinaryExpAST *>(ast)) {
    return evalBinaryExp(binary);
  } else if (auto constExp = dynamic_cast<ast::ConstExpAST *>(ast)) {
    return evalConstExp(constExp);
  } else if (auto lval = dynamic_cast<ast::LValAST *>(ast)) {
    return evalLVal(lval);
  } else {
    assert(false && "Unknown AST type");
  }
}

int32_t Interpreter::evalExpr(ast::ExprAST *expr) const {
  assert(expr && "ExprAST is null");
  return dispatchAndEval(expr->exp.get());
}

int32_t Interpreter::evalConstExp(ast::ConstExpAST *constExp) const {
  assert(constExp && "ConstExpAST is null");
  return dispatchAndEval(constExp->exp.get());
}

int32_t Interpreter::evalPrimaryExp(ast::PrimaryExpAST *primaryExp) const {
  assert(primaryExp && "PrimaryExpAST is null");
  if (primaryExp->isNumber()) {
    return primaryExp->number;
  } else if (primaryExp->isExp()) {
    return dispatchAndEval(primaryExp->exp.get());
  } else if (primaryExp->isLVal()) {
    return dispatchAndEval(primaryExp->lVal.get());
  } else {
    assert(false && "Invalid PrimaryExpAST");
  }
}

int32_t Interpreter::evalUnaryExp(ast::UnaryExpAST *unaryExp) const {
  assert(unaryExp && "UnaryExpAST is null");
  if (unaryExp->isPrimary()) {
    return dispatchAndEval(unaryExp->primaryExp.get());
  } else if (unaryExp->isUnaryOp()) {
    int32_t val = dispatchAndEval(unaryExp->childUnaryExp.get());
    switch (unaryExp->unaryOp) {
      case ast::Op::PLUS:
        return val;
      case ast::Op::MINUS:
        return -val;
      case ast::Op::BANG:
        return !val;
      default:
        assert(false && "Invalid unary operator");
    }
  } else {
    assert(false && "Invalid UnaryExpAST");
  }
}

int32_t Interpreter::evalLVal(ast::LValAST *lval) const {
  return varTables.getConstant(lval->ident);
}

int32_t Interpreter::evalBinaryExp(ast::BinaryExpAST *binaryExp) const {
  assert(binaryExp && "BinaryExpAST is null");
  if (binaryExp->isSingle()) {
    return dispatchAndEval(binaryExp->singleExp.get());
  }

  // Eval LHS and RHS.
  auto &[lhs_ast, rhs_ast] = binaryExp->compositeExp;
  int32_t lhs = dispatchAndEval(lhs_ast.get());
  int32_t rhs = dispatchAndEval(rhs_ast.get());
  switch (binaryExp->binOp) {
    case ast::Op::PLUS:
      return lhs + rhs;
    case ast::Op::MINUS:
      return lhs - rhs;
    case ast::Op::MUL:
      return lhs * rhs;
    case ast::Op::DIV:
      if (rhs == 0) {
        assert(false && "Division by zero");
      }
      return lhs / rhs;
    case ast::Op::MOD:
      if (rhs == 0) {
        assert(false && "Modulo by zero");
      }
      return lhs % rhs;
    case ast::Op::LT:
      return lhs < rhs;
    case ast::Op::GT:
      return lhs > rhs;
    case ast::Op::LE:
      return lhs <= rhs;
    case ast::Op::GE:
      return lhs >= rhs;
    case ast::Op::EQ:
      return lhs == rhs;
    case ast::Op::NEQ:
      return lhs != rhs;
    case ast::Op::LAND:
      return lhs && rhs;
    case ast::Op::LOR:
      return lhs || rhs;
    default:
      assert(false && "Invalid binary operator");
  }
}

} // namespace interpreter
