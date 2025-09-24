#include <cassert>
#include <string>
#include <variant>

#include "AST/AST.h"
#include "AST/Ops.h"
#include "Conversion/IRGen.h"
#include "IR/Function.h"
#include "IR/IRBuilder.h"
#include "IR/Operation.h"
#include "Utils/Utils.h"

using namespace ast;

namespace conversion {

static ir::Type *ASTType2IRType(ir::IRContext &context, ast::Type bType) {
  switch (bType) {
    case ast::Type::INT:
      return ir::IntegerType::get(context, 32);
    default:
      assert(false && "Unsupported AST base type");
      return nullptr;
  }
}

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
  } else if (auto *lval = dynamic_cast<LValAST *>(ast)) {
    return convertLval(builder, lval);
  } else if (auto *init = dynamic_cast<InitValAST *>(ast)) {
    return convertInitValExp(builder, init);
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
  } else if (primaryExpAST->isLVal()) {
    return dispatchAndConvert(builder, primaryExpAST->lVal.get());
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

ir::Value *IRGen::convertLval(ir::IRBuilder &builder, ast::LValAST *lvalAST) {
  assert(lvalAST && "LValAST cannot be null");
  SymbolTable::Val lval = varTables.get(lvalAST->ident);
  if (std::holds_alternative<ir::Value *>(lval)) {
    ir::Value *lvalPtr = std::get<ir::Value *>(lval);
    return builder.create<ir::LoadOp>(lvalPtr)->getResult();
  } else {
    assert(std::holds_alternative<int32_t>(lval) &&
           "LVal symbol table entry must contain either a variable pointer or "
           "constant value");
    int32_t value = std::get<int32_t>(lval);
    return ir::Integer::get(builder.getContext(), value, 32);
  }
}

ir::Value *IRGen::convertInitValExp(ir::IRBuilder &builder,
                                    ast::InitValAST *initValAST) {
  assert(initValAST && "InitValAST cannot be null");
  return dispatchAndConvert(builder, initValAST->exp.get());
}

ir::FunctionType *IRGen::convertFunctionType(FuncTypeAST *funcTypeAST) {
  if (!funcTypeAST || funcTypeAST->type != Type::INT)
    return nullptr;

  auto *intType = ir::IntegerType::get(context, 32);
  return ir::FunctionType::get(context, intType, {});
}

/// Convert statements.
void IRGen::convertStmt(ir::IRBuilder &builder, StmtAST *stmtAST) {
  assert(stmtAST && "Statement AST cannot be null");
  if (auto *returnStmtAST = dynamic_cast<ReturnStmtAST *>(stmtAST)) {
    convertReturnStmt(builder, returnStmtAST);
  } else if (auto *assignStmtAST = dynamic_cast<AssignStmtAST *>(stmtAST)) {
    convertAssignStmt(builder, assignStmtAST);
  } else if (auto *exprStmtAST = dynamic_cast<ExprStmtAST *>(stmtAST)) {
    if (exprStmtAST->exp) {
      dispatchAndConvert(builder, exprStmtAST->exp.get());
    }
  } else if (auto *blockStmtAST = dynamic_cast<BlockStmtAST *>(stmtAST)) {
    convertBlock(builder, blockStmtAST->block.get());
  } else if (auto *ifStmtAST = dynamic_cast<IfStmtAST *>(stmtAST)) {
    convertIfStmt(builder, ifStmtAST);
  } else {
    assert(false && "Unknown statement type");
  }
}

void IRGen::convertReturnStmt(ir::IRBuilder &builder,
                              ReturnStmtAST *returnStmtAST) {
  assert(returnStmtAST && "ReturnStmtAST cannot be null");
  // Void return.
  if (!returnStmtAST->exp) {
    builder.create<ir::ReturnOp>();
  } else {
    // Return with a value.
    ir::Value *returnVal =
        dispatchAndConvert(builder, returnStmtAST->exp.get());
    assert(returnVal && "Return value cannot be null");
    builder.create<ir::ReturnOp>(returnVal);
  }
  // Commit the current block and create an unreachable block to prevent
  // fall-through.
  builder.commitBlock();
  ir::BasicBlock *unreach = ir::BasicBlock::create(
      context, "unreachable_" + std::to_string(getNextBlockId()));
  builder.setInsertPoint(unreach);
}

void IRGen::convertAssignStmt(ir::IRBuilder &builder,
                              AssignStmtAST *assignStmtAST) {
  assert(assignStmtAST && "AssignStmtAST cannot be null");
  auto *lvalAST = dynamic_cast<LValAST *>(assignStmtAST->lVal.get());
  assert(lvalAST && "LValAST cannot be null in assignment");

  SymbolTable::Val lval = varTables.get(lvalAST->ident);
  assert(std::holds_alternative<ir::Value *>(lval) &&
         "LVal must be a variable in assignment");

  ir::Value *lvalPtr = std::get<ir::Value *>(lval);
  ir::Value *rhsValue = dispatchAndConvert(builder, assignStmtAST->exp.get());
  assert(rhsValue && "Right-hand side value cannot be null in assignment");
  builder.create<ir::StoreOp>(rhsValue, lvalPtr);
}

void IRGen::convertIfStmt(ir::IRBuilder &builder, ast::IfStmtAST *ifStmtAST) {
  assert(ifStmtAST && "IfStmtAST cannot be null");
  ir::Value *cond = dispatchAndConvert(builder, ifStmtAST->cond.get());

  // 1. Create `then`, `else` and `end` blocks, this is for easy implementation.
  uint64_t nextID = getNextBlockId();
  ir::BasicBlock *thenBlock =
      ir::BasicBlock::create(context, "then_" + std::to_string(nextID));
  ir::BasicBlock *elseBlock =
      ir::BasicBlock::create(context, "else_" + std::to_string(nextID));
  ir::BasicBlock *endBlock =
      ir::BasicBlock::create(context, "end_" + std::to_string(nextID));
  ir::JumpArg *thenArg = ir::JumpArg::get(context, thenBlock);
  ir::JumpArg *elseArg = ir::JumpArg::get(context, elseBlock);
  builder.create<ir::BranchOp>(cond, thenArg, elseArg);
  builder.commitBlock();

  // 2. Fill in the `then` block.
  builder.setInsertPoint(thenBlock);
  convertStmt(builder, ifStmtAST->thenStmt.get());
  builder.create<ir::JumpOp>(ir::JumpArg::get(context, endBlock));
  builder.commitBlock();

  // 3. Fill in the `else` block if exists, otherwise create a jump to the end.
  builder.setInsertPoint(elseBlock);
  if (ifStmtAST->elseStmt) {
    convertStmt(builder, ifStmtAST->elseStmt.get());
  }
  builder.create<ir::JumpOp>(ir::JumpArg::get(context, endBlock));
  builder.commitBlock();

  // 4. Set the insertion point to the end block.
  builder.setInsertPoint(endBlock);
}

/// Convert declarations and definitions.
void IRGen::convertDeclaration(ir::IRBuilder &builder, ast::DeclAST *declAST) {
  assert(declAST && "Declaration AST cannot be null");
  if (declAST->isVarDecl()) {
    convertVarDecl(builder, dynamic_cast<VarDeclAST *>(declAST->varDecl.get()));
  } else if (declAST->isConstDecl()) {
    convertConstDecl(dynamic_cast<ConstDeclAST *>(declAST->constDecl.get()));
  } else {
    assert(false && "Unknown declaration type");
  }
}

void IRGen::convertConstDecl(ast::ConstDeclAST *constDeclAST) {
  assert(constDeclAST && "ConstDeclAST cannot be null");
  size_t size = constDeclAST->constDefs->size();
  for (size_t i = 0; i < size; ++i) {
    auto *constDef =
        dynamic_cast<ConstDefAST *>((*constDeclAST->constDefs)[i].get());
    convertConstDef(constDef);
  }
}

void IRGen::convertConstDef(ast::ConstDefAST *constDefAST) {
  assert(constDefAST && "ConstDefAST cannot be null");
  auto *constInitVal =
      dynamic_cast<ConstInitValAST *>(constDefAST->initVal.get());
  auto *constExp =
      constInitVal ? dynamic_cast<ConstExpAST *>(constInitVal->constExp.get())
                   : nullptr;
  auto *exp = constExp ? dynamic_cast<ExprAST *>(constExp->exp.get()) : nullptr;
  int32_t value = interpreter.evalExpr(exp);
  varTables.setConstant(constDefAST->var, value);
}

void IRGen::convertVarDecl(ir::IRBuilder &builder,
                           ast::VarDeclAST *varDeclAST) {
  assert(varDeclAST && "VarDeclAST cannot be null");
  size_t size = varDeclAST->varDefs->size();
  for (size_t i = 0; i < size; ++i) {
    auto *varDef = dynamic_cast<VarDefAST *>((*varDeclAST->varDefs)[i].get());
    convertVarDef(builder, varDef, varDeclAST->bType);
  }
}

void IRGen::convertVarDef(ir::IRBuilder &builder, ast::VarDefAST *varDefAST,
                          ast::Type bType) {
  assert(varDefAST && "VarDefAST cannot be null");

  // 1. Create allocation.
  // Since we allow variable shadowing, we use a unique name for each
  // allocation.
  std::string varName = varDefAST->var + "_" + std::to_string(getNextTempId());
  ir::Type *allocType = ASTType2IRType(context, bType);
  ir::Value *alloc =
      builder.create<ir::AllocOp>(varName, allocType, 1)->getResult();
  varTables.setValue(varDefAST->var, alloc);

  // 2. If there is an initializer, evaluate it and store the value.
  if (varDefAST->initVal) {
    ir::Value *initValue =
        dispatchAndConvert(builder, varDefAST->initVal.get());
    builder.create<ir::StoreOp>(initValue, alloc);
  }
}

void IRGen::convertBlock(ir::IRBuilder &builder, BlockAST *blockAST) {
  if (!blockAST)
    return;

  // Enter a new scope for the block.
  varTables.enterScope();

  size_t size = blockAST->blockItems->size();
  for (size_t i = 0; i < size; ++i) {
    BlockItemAST *item = (*blockAST->blockItems)[i].get();
    if (item->isStmt()) {
      convertStmt(builder, dynamic_cast<StmtAST *>(item->stmt.get()));
    } else if (item->isDecl()) {
      convertDeclaration(builder, dynamic_cast<DeclAST *>(item->decl.get()));
    } else {
      assert(false && "Unknown block item type");
    }
  }
  // Exit the scope after finishing the block.
  varTables.exitScope();
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

  ir::Function *function =
      ir::Function::create(context, funcDef->ident, funcType);

  ir::IRBuilder builder(context);
  builder.setCurrentFunction(function);
  ir::BasicBlock *entryBlock = ir::BasicBlock::create(context, "entry");
  builder.setInsertPoint(entryBlock);
  convertBlock(builder, dynamic_cast<BlockAST *>(funcDef->block.get()));
  builder.commitBlock();

  return function;
}

} // namespace conversion
