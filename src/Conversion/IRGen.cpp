#include <cassert>
#include <functional>
#include <numeric>
#include <string>
#include <variant>
#include <vector>

#include "AST/AST.h"
#include "AST/Ops.h"
#include "Conversion/IRGen.h"
#include "IR/BasicBlock.h"
#include "IR/Function.h"
#include "IR/IRBuilder.h"
#include "IR/Module.h"
#include "IR/Operation.h"
#include "IR/Value.h"
#include "Utils/Utils.h"

using namespace ast;

namespace conversion {

/// Helper functions.
void updateIndicesWithAlign(const std::vector<size_t> &dims,
                            std::vector<size_t> &indices, size_t &alignDim) {
  for (int64_t i = dims.size() - 1; i >= 0; --i) {
    indices[i]++;
    if (indices[i] < dims[i]) {
      break;
    } else if (i > 0) {
      indices[i] = 0;
      alignDim = i - 1;
    }
  }
}

/// Convert AST Type to IR Type.
/// If the type is void, return std::nullopt.
static std::optional<ir::Type *> ASTType2IRType(ir::IRContext &context,
                                                const Type &type) {
  switch (type.kind) {
    case Type::Kind::BASE: {
      switch (type.base) {
        case BaseType::INT:
          return ir::IntegerType::get(context, 32);
        case BaseType::VOID:
          return std::nullopt; // Void type represented as nullopt.
        default:
          assert(false && "Unsupported AST base type");
          return std::nullopt;
      }
    }
    case Type::Kind::POINTER: {
      if (!type.pointee) {
        return std::nullopt; // Malformed AST: missing pointee type.
      }
      auto pointeeTypeOpt = ASTType2IRType(context, *type.pointee);
      if (!pointeeTypeOpt.has_value()) {
        return std::nullopt; // Void pointer represented as nullopt.
      }
      return ir::PointerType::get(context, pointeeTypeOpt.value());
    }
  }
  assert(false && "Unsupported AST type kind");
}

ir::Value *IRGen::computeArrayElementPtr(ir::IRBuilder &builder,
                                         ir::Value *basePtr,
                                         const std::vector<ExprPtr> &indices) {
  assert(!indices.empty() && "Array indices cannot be empty");
  std::vector<ir::Value *> indexValues;
  for (const auto &indexExpr : indices) {
    ir::Value *indexValue = dispatchAndConvert(builder, indexExpr.get());
    indexValues.push_back(indexValue);
  }

  // Chain together GetElemPtr operations to calculate the final element
  // pointer.
  ir::Value *currentPtr = basePtr;
  for (const auto &indexValue : indexValues) {
    currentPtr =
        builder.create<ir::GetElemPtrOp>(currentPtr, indexValue)->getResult();
  }
  return currentPtr;
}

std::vector<size_t>
IRGen::evaluateArraySizes(std::vector<ConstExpPtr> &arraySizes) {
  std::vector<size_t> sizes;
  for (const auto &sizeExp : arraySizes) {
    int32_t sizeValue = interpreter.evalConstExp(sizeExp.get());
    assert(sizeValue > 0 && "Array size must be positive");
    sizes.push_back(static_cast<size_t>(sizeValue));
  }
  return sizes;
}

template <typename NodeType, typename GetChildrenFunc,
          typename ValueExtractorType>
void IRGen::flattenGenericNode(ir::IRBuilder &builder, NodeType *node,
                               size_t &alignDim,
                               const std::vector<size_t> &dims,
                               std::vector<size_t> &indices,
                               std::vector<ir::Value *> &result,
                               const GetChildrenFunc &getChildren,
                               const ValueExtractorType &valueExtractor) {
  if (node->isScalar()) {
    result.push_back(valueExtractor(builder, node));
    updateIndicesWithAlign(dims, indices, alignDim);
    return;
  }

  assert(node->isArray() && "Node must be either scalar or array");
  size_t currentAlignDim = alignDim;
  size_t nextAlignIndex = indices[currentAlignDim] + 1;

  for (auto &child : *getChildren(node)) {
    flattenGenericNode(builder, child.get(), alignDim, dims, indices, result,
                       getChildren, valueExtractor);
  }

  // Fill zero for remaining elements.
  ir::Value *zero = ir::Integer::get(context, 0, 32);
  while (alignDim >= currentAlignDim &&
         indices[currentAlignDim] < nextAlignIndex) {
    result.push_back(zero);
    updateIndicesWithAlign(dims, indices, alignDim);
  }
}

template <typename NodeType, typename ContainerType, typename GetChildrenFunc,
          typename ValueExtractorType>
void IRGen::flattenGenericInitList(ir::IRBuilder &builder,
                                   ContainerType &initList,
                                   const std::vector<size_t> &dims,
                                   std::vector<ir::Value *> &result,
                                   const GetChildrenFunc &getChildren,
                                   const ValueExtractorType &valueExtractor) {
  assert(!dims.empty() && "Array must have at least one dimension");
  result.clear();
  std::vector<size_t> indices(dims.size(), 0);
  size_t outerDim = dims[0];
  size_t alignDim = 0; // Start aligning from the outermost dimension.

  for (auto &node : initList) {
    flattenGenericNode<NodeType>(builder, node.get(), alignDim, dims, indices,
                                 result, getChildren, valueExtractor);
  }
  ir::Value *zero = ir::Integer::get(context, 0, 32);
  while (indices[0] < outerDim) {
    result.push_back(zero);
    updateIndicesWithAlign(dims, indices, alignDim);
  }

  // Sanity check.
  size_t expectedSize = std::accumulate(dims.begin(), dims.end(), 1ULL,
                                        std::multiplies<size_t>());
  assert(result.size() == expectedSize &&
         "Flattened initializer list size does not match expected array size");
}

void IRGen::flattenInitList(ir::IRBuilder &builder,
                            std::vector<InitValASTPtr> &initList,
                            const std::vector<size_t> &dims,
                            std::vector<ir::Value *> &result) {
  auto extractValue = [this](ir::IRBuilder &builder,
                             InitValAST *node) -> ir::Value * {
    return dispatchAndConvert(builder, node->exp.get());
  };
  auto getChildren = [](InitValAST *node) { return node->initValVec.get(); };
  flattenGenericInitList<InitValAST>(builder, initList, dims, result,
                                     getChildren, extractValue);
}

void IRGen::flattenConstInitList(ir::IRBuilder &builder,
                                 std::vector<ast::ConstInitValASTPtr> &initList,
                                 const std::vector<size_t> &dims,
                                 std::vector<ir::Value *> &result) {
  auto extractConstValue = [this](ir::IRBuilder &builder,
                                  ConstInitValAST *node) -> ir::Value * {
    int32_t value = interpreter.evalConstExp(node->constExp.get());
    return ir::Integer::get(context, value);
  };
  auto getChildren = [](ConstInitValAST *node) {
    return node->constInitVec.get();
  };
  flattenGenericInitList<ast::ConstInitValAST>(builder, initList, dims, result,
                                               getChildren, extractConstValue);
}

ir::Value *IRGen::dispatchAndConvert(ir::IRBuilder &builder, BaseAST *ast) {
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
                                   BinaryExpAST *binaryExpAST) {
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
                                          BinaryExpAST *binaryExpAST) {
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
      builder.create<ir::LocalAlloc>("", intType, false)->getResult();

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

ir::Value *IRGen::convertLval(ir::IRBuilder &builder, LValAST *lvalAST) {
  assert(lvalAST && "LValAST cannot be null");
  SymbolTable::Val lval = varTables.get(lvalAST->ident);
  if (std::holds_alternative<ir::Value *>(lval)) {
    ir::Value *lvalPtr = std::get<ir::Value *>(lval);

    // If there are indices, we need to compute the address using GetElemPtrOp.
    if (lvalAST->isScalar()) {
      return builder.create<ir::LoadOp>(lvalPtr)->getResult();
    }
    assert(lvalAST->isArray() && "LVal must be either scalar or array");
    ir::Value *elementPtr =
        computeArrayElementPtr(builder, lvalPtr, *(lvalAST->arrayIndices));
    return builder.create<ir::LoadOp>(elementPtr)->getResult();
  } else {
    assert(std::holds_alternative<int32_t>(lval) &&
           "LVal symbol table entry must contain either a variable pointer or "
           "constant value");
    int32_t value = std::get<int32_t>(lval);
    return ir::Integer::get(builder.getContext(), value, 32);
  }
}

ir::Value *IRGen::convertFuncCall(ir::IRBuilder &builder,
                                  FuncCallAST *funcCallAST) {
  assert(funcCallAST && "FuncCallAST cannot be null");
  auto *funcDecl = dynamic_cast<ir::FunctionDecl *>(
      varTables.getDeclaration(funcCallAST->ident));
  assert(funcDecl && "Function declaration not found in symbol table");
  ir::FunctionType *funcType = funcDecl->getFunctionType();

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
                                    InitValAST *initValAST) {
  assert(initValAST && "InitValAST cannot be null");
  return dispatchAndConvert(builder, initValAST->exp.get());
}

ir::FunctionType *
IRGen::convertFunctionType(const Type &retType,
                           const std::vector<const Type *> &paramTypes) {
  std::optional<ir::Type *> mayIrRetType = ASTType2IRType(context, retType);
  std::vector<ir::Type *> irParamTypes;
  for (const auto &paramType : paramTypes) {
    ir::Type *irType = ASTType2IRType(context, *paramType).value();
    irParamTypes.push_back(irType);
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
  ir::Value *targetPtr = std::get<ir::Value *>(lval);
  assert(targetPtr && "Target pointer cannot be null in assignment");

  // Get right-hand side value and store it to the target.
  ir::Value *rhsValue = dispatchAndConvert(builder, assignStmtAST->exp.get());
  assert(rhsValue && "Right-hand side value cannot be null in assignment");

  if (lvalAST->isScalar()) {
    builder.create<ir::StoreOp>(rhsValue, targetPtr);
    return;
  }

  // Array assignment.
  if (lvalAST->isArray()) {
    ir::Value *elementPtr =
        computeArrayElementPtr(builder, targetPtr, *(lvalAST->arrayIndices));
    builder.create<ir::StoreOp>(rhsValue, elementPtr);
    return;
  }
  assert(false && "LVal must be either scalar or array in assignment");
}

void IRGen::convertIfStmt(ir::IRBuilder &builder, IfStmtAST *ifStmtAST) {
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
                             WhileStmtAST *whileStmtAST) {
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
                             BreakStmtAST *breakStmtAST) {
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
                                ContinueStmtAST *continueStmtAST) {
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
void IRGen::convertDeclaration(ir::IRBuilder &builder, DeclAST *declAST,
                               bool isGlobal) {
  assert(declAST && "Declaration AST cannot be null");
  if (declAST->isVarDecl()) {
    convertVarDecl(builder, dynamic_cast<VarDeclAST *>(declAST->varDecl.get()),
                   isGlobal);
  } else if (declAST->isConstDecl()) {
    convertConstDecl(builder,
                     dynamic_cast<ConstDeclAST *>(declAST->constDecl.get()),
                     isGlobal);
  } else {
    assert(false && "Unknown declaration type");
  }
}

void IRGen::convertConstDecl(ir::IRBuilder &builder, ConstDeclAST *constDeclAST,
                             bool isGlobal) {
  assert(constDeclAST && "ConstDeclAST cannot be null");
  size_t size = constDeclAST->constDefs->size();
  ir::Type *bType = ASTType2IRType(context, *constDeclAST->bType).value();
  for (size_t i = 0; i < size; ++i) {
    auto *constDef =
        dynamic_cast<ConstDefAST *>((*constDeclAST->constDefs)[i].get());
    convertConstDef(builder, constDef, bType, isGlobal);
  }
}

void IRGen::convertConstDef(ir::IRBuilder &builder, ConstDefAST *constDefAST,
                            ir::Type *bType, bool isGlobal) {
  assert(constDefAST && "ConstDefAST cannot be null");
  ConstInitValAST *constInitVal = constDefAST->initVal.get();
  assert(constInitVal && "ConstDef must have an initializer");

  if (constDefAST->isScalar()) {
    assert(constInitVal->isScalar() &&
           "Scalar ConstDef must have scalar initializer");
    int32_t value = interpreter.evalExpr(
        dynamic_cast<ExprAST *>(constInitVal->constExp->exp.get()));
    varTables.setConstant(constDefAST->var, value);
    return;
  }
  if (constDefAST->isArray()) {
    assert(constInitVal->isArray() &&
           "Array ConstDef must have array initializer");
    // Compute array size.
    std::vector<size_t> arraySizes =
        evaluateArraySizes(*constDefAST->arraySizes.get());
    ir::ArrayType *arrayType = ir::ArrayType::get(context, bType, arraySizes);

    // Distinguish between global and local array constant definitions.
    if (isGlobal) {
      std::string varName = constDefAST->var;
      std::vector<ir::Value *> initValues;
      flattenConstInitList(builder, *constInitVal->constInitVec, arraySizes,
                           initValues);
      auto *alloc =
          ir::GlobalAlloc::create(context, varName, arrayType, initValues);
      auto *constDecl = ir::GlobalVarDecl::create(context, varName, alloc);
      currentModule->addDeclaration(constDecl);
      varTables.setValue(constDefAST->var, alloc->getResult());
    } else {
      std::string varName =
          constDefAST->var + "_" + std::to_string(getNextTempId());
      ir::Value *alloc =
          builder.create<ir::LocalAlloc>(varName, arrayType, true)->getResult();
      varTables.setValue(constDefAST->var, alloc);
      std::vector<ir::Value *> initValues;
      flattenConstInitList(builder, *constInitVal->constInitVec, arraySizes,
                           initValues);
      initializeLocalArray(builder, alloc, arraySizes, initValues);
    }
    return;
  }

  assert(false && "Unknown ConstDef type");
}

void IRGen::convertVarDecl(ir::IRBuilder &builder, VarDeclAST *varDeclAST,
                           bool isGlobal) {
  assert(varDeclAST && "VarDeclAST cannot be null");
  size_t size = varDeclAST->varDefs->size();
  for (size_t i = 0; i < size; ++i) {
    auto *varDef = dynamic_cast<VarDefAST *>((*varDeclAST->varDefs)[i].get());
    if (isGlobal) {
      convertGlobalVarDef(builder, varDef, *varDeclAST->bType.get());
    } else {
      convertLocalVarDef(builder, varDef, *varDeclAST->bType.get());
    }
  }
}

void IRGen::convertLocalVarDef(ir::IRBuilder &builder, VarDefAST *varDefAST,
                               const Type &bType) {
  assert(varDefAST && "VarDefAST cannot be null");

  // 1. Create allocation.
  // Since we allow variable shadowing, we use a unique name for each
  // allocation.
  std::string varName = varDefAST->var + "_" + std::to_string(getNextTempId());
  ir::Type *allocType = ASTType2IRType(context, bType).value();

  if (varDefAST->isScalar()) {
    ir::Value *alloc =
        builder.create<ir::LocalAlloc>(varName, allocType, true)->getResult();
    varTables.setValue(varDefAST->var, alloc);

    // If there is an initializer, evaluate it and store the value.
    if (varDefAST->initVal) {
      ir::Value *initValue =
          dispatchAndConvert(builder, varDefAST->initVal.get());
      builder.create<ir::StoreOp>(initValue, alloc);
    }
    return;
  }

  if (varDefAST->isArray()) {
    assert(varDefAST->arraySizes && "Array variable must have a size");
    std::vector<size_t> arraySizes =
        evaluateArraySizes(*varDefAST->arraySizes.get());
    ir::ArrayType *arrayType =
        ir::ArrayType::get(context, allocType, arraySizes);

    ir::Value *alloc =
        builder.create<ir::LocalAlloc>(varName, arrayType, true)->getResult();
    varTables.setValue(varDefAST->var, alloc);

    if (!varDefAST->initVal)
      return;

    // If there is an initializer, evaluate it and store the values.
    InitValAST *initValAST = varDefAST->initVal.get();
    assert(initValAST->isArray() &&
           "Array variable must have array initializer");
    std::vector<ir::Value *> flattenedInits;
    flattenInitList(builder, *varDefAST->initVal->initValVec.get(), arraySizes,
                    flattenedInits);
    initializeLocalArray(builder, alloc, arraySizes, flattenedInits);
    return;
  }

  assert(false && "Unknown VarDef type");
}

void IRGen::initializeLocalArray(ir::IRBuilder &builder, ir::Value *alloc,
                                 const std::vector<size_t> &dims,
                                 const std::vector<ir::Value *> &initValues) {
  assert(alloc && "Array allocation cannot be null");
  assert(!dims.empty() && "Array dimensions cannot be empty");
  size_t totalSize = std::accumulate(dims.begin(), dims.end(), 1ULL,
                                     std::multiplies<size_t>());

  // The initialization values should already be flattened.
  assert(initValues.size() == totalSize &&
         "Initializer size does not match array size");
  std::vector<size_t> indices(dims.size(), 0);

  auto updateIndices = [&]() {
    for (int64_t i = dims.size() - 1; i >= 0; --i) {
      indices[i]++;
      if (indices[i] < dims[i]) {
        break;
      } else if (i > 0) {
        indices[i] = 0;
      }
    }
  };

  for (size_t i = 0; i < totalSize; ++i) {
    ir::Value *initValue = initValues[i];
    // Compute the element pointer using the current indices.
    ir::Value *elemPtr = alloc;
    for (size_t j = 0; j < dims.size(); ++j) {
      ir::Integer *idx =
          ir::Integer::get(context, static_cast<int>(indices[j]), 32);
      elemPtr = builder.create<ir::GetElemPtrOp>(elemPtr, idx)->getResult();
    }
    builder.create<ir::StoreOp>(initValue, elemPtr);
    updateIndices();
  }
}

void IRGen::convertGlobalVarDef(ir::IRBuilder &builder, VarDefAST *varDefAST,
                                const Type &bType) {
  assert(varDefAST && "VarDefAST cannot be null");
  std::string varName = varDefAST->var;

  auto registerGlobalVar = [&](ir::GlobalAlloc *alloc) {
    auto *varDecl = ir::GlobalVarDecl::create(context, varName, alloc);
    currentModule->addDeclaration(varDecl);
    varTables.setValue(varName, alloc->getResult());
  };

  // 1. Create allocation.
  ir::Type *allocType = ASTType2IRType(context, bType).value();

  if (varDefAST->isScalar()) {
    ir::Value *initValue = nullptr;
    if (varDefAST->initVal) {
      // The builder here is only used to dispatch and convert the initializer.
      // We assume the initializer is a constant expression for global
      // variables.
      initValue = dispatchAndConvert(builder, varDefAST->initVal.get());
    }

    auto *alloc =
        ir::GlobalAlloc::create(context, varName, allocType, initValue);
    registerGlobalVar(alloc);
    return;
  }

  if (varDefAST->isArray()) {
    assert(varDefAST->arraySizes && "Array variable must have a size");
    std::vector<size_t> arraySizes =
        evaluateArraySizes(*varDefAST->arraySizes.get());
    auto *arrayType = ir::ArrayType::get(context, allocType, arraySizes);
    std::vector<ir::Value *> initValues;
    if (varDefAST->initVal) {
      InitValAST *initValAST = varDefAST->initVal.get();
      assert(initValAST->isArray() &&
             "Array variable must have array initializer");
      flattenInitList(builder, *initValAST->initValVec.get(), arraySizes,
                      initValues);
    }
    auto *array =
        ir::GlobalAlloc::create(context, varName, arrayType, initValues);
    registerGlobalVar(array);
    return;
  }

  assert(false && "Unknown VarDef type");
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

ir::Function *IRGen::convertFuncDef(FuncDefAST *funcDefAST) {
  assert(funcDefAST && "Function definition AST cannot be null");
  auto funcType = convertFunctionType(*funcDefAST->getReturnType(),
                                      funcDefAST->getParamTypes());
  ir::FunctionDecl *funcDecl =
      ir::FunctionDecl::create(context, funcDefAST->ident, funcType);
  varTables.setDeclaration(funcDefAST->ident, funcDecl);

  ir::Function *function = ir::Function::create(
      context, funcDefAST->ident, funcDefAST->getParamNames(), funcType);

  ir::IRBuilder builder(context);
  builder.setCurrentFunction(function);
  ir::BasicBlock *entryBlock = ir::BasicBlock::create(
      context, "entry" + std::to_string(getNextBlockId()));
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
        builder.create<ir::LocalAlloc>("", type, false)->getResult();
    // Mapping the argument to the allocation.
    varTables.setValue(arg->getName(), alloc);
    // 2. Store the argument value into the allocation.
    builder.create<ir::StoreOp>(arg, alloc);
  }
}

/// Add declarations for built-in library functions.
struct LibFunc {
  std::string name;
  std::vector<const Type *> paramTypes;
  const Type *returnType;
};

// Accessor functions for AST type definitions to avoid static initialization
// order issues.
static const Type *getASTIntType() {
  static const Type instance(BaseType::INT);
  return &instance;
}

static const Type *getASTVoidType() {
  static const Type instance(BaseType::VOID);
  return &instance;
}

static const Type *getASTIntPtrType() {
  static const Type instance(getASTIntType());
  return &instance;
}

static const std::vector<LibFunc> libFuncs = {
    {"getint", {}, getASTIntType()},
    {"getch", {}, getASTIntType()},
    {"getarray", {getASTIntPtrType()}, getASTVoidType()},
    {"putint", {getASTIntType()}, getASTVoidType()},
    {"putch", {getASTIntType()}, getASTVoidType()},
    {"putarray", {getASTIntType(), getASTIntPtrType()}, getASTVoidType()},
    {"starttime", {}, getASTVoidType()},
    {"stoptime", {}, getASTVoidType()}};

void IRGen::addLibFuncDeclarations(ir::Module *module) {
  assert(module && "Module cannot be null");
  for (const LibFunc &libFunc : libFuncs) {
    auto funcType =
        convertFunctionType(*libFunc.returnType, libFunc.paramTypes);
    ir::FunctionDecl *funcDecl =
        ir::FunctionDecl::create(context, libFunc.name, funcType);
    module->addDeclaration(funcDecl);
    varTables.setDeclaration(libFunc.name, funcDecl);
  }
}

ir::Module *IRGen::generate(std::unique_ptr<BaseAST> &ast) {
  assert(ast && "AST cannot be null");
  auto *compUnit = dynamic_cast<CompUnitAST *>(ast.get());
  assert(compUnit && "AST must be a compilation unit");
  currentModule = ir::Module::create(context);

  // Create a global scope for the compilation unit.
  varTables.enterScope();

  // Add built-in library function declarations.
  addLibFuncDeclarations(currentModule);

  for (auto &node : *compUnit->topLevelNodes) {
    if (auto *func = dynamic_cast<FuncDefAST *>(node.get())) {
      ir::Function *function = convertFuncDef(func);
      currentModule->addFunction(function);
      resetTempId();
    } else if (auto *decl = dynamic_cast<DeclAST *>(node.get())) {
      ir::IRBuilder builder(context);
      // Global declarations.
      convertDeclaration(builder, decl, true);
    } else {
      assert(false && "Unknown top-level node type");
    }
  }
  // Exit the global scope.
  varTables.exitScope();

  return currentModule;
}

} // namespace conversion
