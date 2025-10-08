#include <cassert>
#include <string>
#include <variant>

#include "AST/AST.h"
#include "AST/Ops.h"
#include "Conversion/IRGen.h"
#include "IR/BasicBlock.h"
#include "IR/Function.h"
#include "IR/IRBuilder.h"
#include "IR/Module.h"
#include "IR/Operation.h"
#include "Utils/Utils.h"

using namespace ast;

namespace conversion {

static ir::Type *ASTType2IRBType(ir::IRContext &context, ast::Type bType) {
  switch (bType) {
    case ast::Type::INT:
      return ir::IntegerType::get(context, 32);
    default:
      assert(false && "Unsupported AST base type");
      return nullptr;
  }
}

static std::optional<ir::Type *> ASTType2IRType(ir::IRContext &context,
                                                ast::Type type) {
  switch (type) {
    case ast::Type::INT:
      return ir::IntegerType::get(context, 32);
    case ast::Type::VOID:
      return std::nullopt; // Void type represented as nullopt.
    default:
      assert(false && "Unsupported AST base type");
      return std::nullopt;
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
  } else if (unaryExprAST->isFuncCall()) {
    return convertFuncCall(builder, unaryExprAST->funcCall.get());
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
    if (isLogicalOp(binaryExpAST->binOp)) {
      return convertLogicalBinaryExp(builder, binaryExpAST);
    }

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
      default:
        assert(false && "Unknown binary operator");
        return nullptr;
    }
  } else {
    assert(false && "Unknown expression type");
  }
}

ir::Value *IRGen::convertLogicalBinaryExp(ir::IRBuilder &builder,
                                          ast::BinaryExpAST *binaryExpAST) {
  assert(binaryExpAST && "Binary expression AST cannot be null");
  auto op = binaryExpAST->binOp;

  // Compute lhs first.
  ir::Value *lhs =
      dispatchAndConvert(builder, binaryExpAST->compositeExp.first.get());
  ir::Integer *zero = ir::Integer::get(builder.getContext(), 0, 32);
  ir::IntegerType *intType = ir::IntegerType::get(context, 32);

  const std::string prefix = (op == Op::LAND) ? "land" : "lor";
  ir::Value *earlyValue = (op == Op::LAND) ? ir::Integer::get(context, 0)
                                           : ir::Integer::get(context, 1);
  // The name here is irrelevant since it's a temporary variable.
  // IRPrinter will take care of printing it properly.
  ir::Value *result =
      builder.create<ir::AllocOp>("", intType, false)->getResult();

  uint64_t nextID = getNextBlockId();
  ir::BasicBlock *early = ir::BasicBlock::create(
      context, prefix + "_early_" + std::to_string(nextID));
  ir::BasicBlock *cont = ir::BasicBlock::create(
      context, prefix + "_cont_" + std::to_string(nextID));
  ir::BasicBlock *end = ir::BasicBlock::create(
      context, prefix + "_end_" + std::to_string(nextID));

  if (op == Op::LAND) {
    builder.create<ir::CondBranchOp>(lhs, cont, early);
  } else {
    builder.create<ir::CondBranchOp>(lhs, early, cont);
  }
  builder.commitBlock();

  // Fill in the `early`, `cont` and `end` blocks.
  builder.setInsertPoint(early);
  builder.create<ir::StoreOp>(earlyValue, result);
  builder.create<ir::JumpOp>(end);
  builder.commitBlock();

  builder.setInsertPoint(cont);
  ir::Value *rhs =
      dispatchAndConvert(builder, binaryExpAST->compositeExp.second.get());
  ir::Value *rhsBool = builder.create<ir::NeqOp>(rhs, zero)->getResult();
  builder.create<ir::StoreOp>(rhsBool, result);
  builder.create<ir::JumpOp>(end);
  builder.commitBlock();

  builder.setInsertPoint(end);
  return builder.create<ir::LoadOp>(result)->getResult();
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

ir::Value *IRGen::convertFuncCall(ir::IRBuilder &builder,
                                  ast::FuncCallAST *funcCallAST) {
  assert(funcCallAST && "FuncCallAST cannot be null");
  ir::FunctionType *funcType = varTables.getFunctionType(funcCallAST->ident);

  // Prepare arguments.
  std::vector<ir::Value *> args;
  if (funcCallAST->hasParams()) {
    for (const auto &param : *(funcCallAST->funcRParams)) {
      args.push_back(dispatchAndConvert(builder, param->exp.get()));
    }
  }

  if (funcType->hasReturnType()) {
    return builder
        .create<ir::CallOp>(funcCallAST->ident, args, funcType->getReturnType())
        ->getResult();
  } else {
    builder.create<ir::CallOp>(funcCallAST->ident, args, nullptr);
    return nullptr; // Void function call returns no value.
  }
}

ir::Value *IRGen::convertInitValExp(ir::IRBuilder &builder,
                                    ast::InitValAST *initValAST) {
  assert(initValAST && "InitValAST cannot be null");
  return dispatchAndConvert(builder, initValAST->exp.get());
}

ir::FunctionType *
IRGen::convertFunctionType(ast::Type retType,
                           const std::vector<ast::Type> &paramTypes) {
  std::optional<ir::Type *> mayIrRetType = ASTType2IRType(context, retType);
  std::vector<ir::Type *> irParamTypes;
  for (const auto &paramType : paramTypes) {
    irParamTypes.push_back(ASTType2IRBType(context, paramType));
  }
  return ir::FunctionType::get(context, mayIrRetType, irParamTypes);
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
  } else if (auto *whileStmtAST = dynamic_cast<WhileStmtAST *>(stmtAST)) {
    convertWhileStmt(builder, whileStmtAST);
  } else if (auto *breakStmtAST = dynamic_cast<BreakStmtAST *>(stmtAST)) {
    convertBreakStmt(builder, breakStmtAST);
  } else if (auto *continueStmtAST = dynamic_cast<ContinueStmtAST *>(stmtAST)) {
    convertContinueStmt(builder, continueStmtAST);
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
  createUnreachableBlock(builder);
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
  builder.create<ir::CondBranchOp>(cond, thenBlock, elseBlock);
  builder.commitBlock();

  // 2. Fill in the `then` block.
  builder.setInsertPoint(thenBlock);
  convertStmt(builder, ifStmtAST->thenStmt.get());
  builder.create<ir::JumpOp>(endBlock);
  builder.commitBlock();

  // 3. Fill in the `else` block if exists, otherwise create a jump to the end.
  builder.setInsertPoint(elseBlock);
  if (ifStmtAST->elseStmt) {
    convertStmt(builder, ifStmtAST->elseStmt.get());
  }
  builder.create<ir::JumpOp>(endBlock);
  builder.commitBlock();

  // 4. Set the insertion point to the end block.
  builder.setInsertPoint(endBlock);
}

void IRGen::convertWhileStmt(ir::IRBuilder &builder,
                             ast::WhileStmtAST *whileStmtAST) {
  assert(whileStmtAST && "WhileStmtAST cannot be null");
  uint64_t nextID = getNextBlockId();
  ir::BasicBlock *entry =
      ir::BasicBlock::create(context, "while_entry_" + std::to_string(nextID));
  ir::BasicBlock *body =
      ir::BasicBlock::create(context, "while_body_" + std::to_string(nextID));
  ir::BasicBlock *end =
      ir::BasicBlock::create(context, "while_end_" + std::to_string(nextID));

  // Jump to the entry block.
  builder.create<ir::JumpOp>(entry);
  builder.commitBlock();

  // Fill in the entry block.
  builder.setInsertPoint(entry);
  ir::Value *cond = dispatchAndConvert(builder, whileStmtAST->cond.get());
  builder.create<ir::CondBranchOp>(cond, body, end);
  builder.commitBlock();

  // Fill in the body block.
  builder.setInsertPoint(body);
  // Push the loop pair onto the stack.
  loopStack.push_front({entry, end});
  convertStmt(builder, whileStmtAST->body.get());
  builder.create<ir::JumpOp>(entry);
  builder.commitBlock();

  // Set the insertion point to the end block.
  builder.setInsertPoint(end);
  // Pop the loop pair from the stack.
  loopStack.pop_front();
}

void IRGen::convertBreakStmt(ir::IRBuilder &builder,
                             ast::BreakStmtAST *breakStmtAST) {
  assert(breakStmtAST && "BreakStmtAST cannot be null");
  assert(!loopStack.empty() && "Break statement not within a loop");

  // Jump to the exit block of the innermost loop.
  ir::BasicBlock *exitBlock = loopStack.front().second;
  builder.create<ir::JumpOp>(exitBlock);
  builder.commitBlock();

  // Create an unreachable block to prevent fall-through.
  createUnreachableBlock(builder);
}

void IRGen::convertContinueStmt(ir::IRBuilder &builder,
                                ast::ContinueStmtAST *continueStmtAST) {
  assert(continueStmtAST && "ContinueStmtAST cannot be null");
  assert(!loopStack.empty() && "Continue statement not within a loop");

  // Jump to the entry block of the innermost loop.
  ir::BasicBlock *entryBlock = loopStack.front().first;
  builder.create<ir::JumpOp>(entryBlock);
  builder.commitBlock();

  // Create an unreachable block to prevent fall-through.
  createUnreachableBlock(builder);
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
  ir::Type *allocType = ASTType2IRBType(context, bType);
  ir::Value *alloc =
      builder.create<ir::AllocOp>(varName, allocType, true)->getResult();
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

void IRGen::createUnreachableBlock(ir::IRBuilder &builder) {
  ir::BasicBlock *unreach = ir::BasicBlock::create(
      context, "unreachable_" + std::to_string(getNextBlockId()));
  builder.setInsertPoint(unreach);
}

ir::Function *IRGen::convertFuncDef(ast::FuncDefAST *funcDefAST) {
  assert(funcDefAST && "Function definition AST cannot be null");
  auto funcType = convertFunctionType(funcDefAST->getReturnType(),
                                      funcDefAST->getParamTypes());
  varTables.setFunctionType(funcDefAST->ident, funcType);

  ir::Function *function = ir::Function::create(
      context, funcDefAST->ident, funcDefAST->getParamNames(), funcType);

  ir::IRBuilder builder(context);
  builder.setCurrentFunction(function);
  ir::BasicBlock *entryBlock = ir::BasicBlock::create(context, "entry");
  builder.setInsertPoint(entryBlock);
  // Enter a new scope for the function arguments.
  varTables.enterScope();

  prepareFunctionArgs(builder, function);
  convertBlock(builder, dynamic_cast<BlockAST *>(funcDefAST->block.get()));

  // If the function does not end with a return, add a return instruction.
  ir::BasicBlock *currentBlock = builder.getCurrentBlock();
  if (!currentBlock->isTerminated()) {
    if (!function->getFunctionType()->hasReturnType()) {
      builder.create<ir::ReturnOp>();
    } else {
      assert(function->getFunctionType()->getReturnType()->isInteger() &&
             "Only integer return type is supported");
      // Return 0 by default for functions with integer return type.
      ir::Integer *zero = ir::Integer::get(context, 0, 32);
      builder.create<ir::ReturnOp>(zero);
    }
  }
  builder.commitBlock();

  // Exit the function scope.
  varTables.exitScope();
  return function;
}

void IRGen::prepareFunctionArgs(ir::IRBuilder &builder,
                                ir::Function *function) {
  assert(function && "Function cannot be null");
  if (!function->hasArgs()) {
    return;
  }
  for (ir::FuncArg *arg : function->getArgs()) {
    // 1. Create an allocation for the argument.
    auto *type = dynamic_cast<ir::IntegerType *>(arg->getType());
    if (!type) {
      continue;
    }
    ir::Value *alloc =
        builder.create<ir::AllocOp>("", type, false)->getResult();
    // Mapping the argument to the allocation.
    varTables.setValue(arg->getName(), alloc);
    // 2. Store the argument value into the allocation.
    builder.create<ir::StoreOp>(arg, alloc);
  }
}

ir::Module *IRGen::generate(std::unique_ptr<ast::BaseAST> &ast) {
  assert(ast && "AST cannot be null");
  auto *compUnit = dynamic_cast<CompUnitAST *>(ast.get());
  assert(compUnit && "AST must be a compilation unit");
  ir::Module *module = ir::Module::create(context);

  // Create a global scope for the compilation unit.
  varTables.enterScope();

  for (auto &funcDefPtr : *compUnit->funcDefs) {
    ir::Function *function =
        convertFuncDef(dynamic_cast<FuncDefAST *>(funcDefPtr.get()));
    module->addFunction(function);
  }
  // Exit the global scope.
  varTables.exitScope();

  return module;
}

} // namespace conversion
