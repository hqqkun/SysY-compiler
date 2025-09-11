#include "Conversion/Conversion.h"
#include "IR/Operation.h"

using namespace ast;

namespace conversion {

static ir::FunctionType *convertFunctionType(ir::IRContext &context,
                                             FuncTypeAST *funcTypeAST) {
  if (!funcTypeAST || funcTypeAST->type != Type::INT)
    return nullptr;

  auto *intType = ir::IntegerType::get(context, 32);
  return ir::FunctionType::get(context, intType, {});
}

static ir::Operation *convertStmt(ir::IRContext &context, StmtAST *stmtAST) {
  return stmtAST ? context.create<ir::ReturnOp>(
                       ir::Integer::get(context, stmtAST->number))
                 : nullptr;
}

static ir::BasicBlock *convertBlock(ir::IRContext &context,
                                    BlockAST *blockAST) {
  if (!blockAST)
    return nullptr;

  ir::BasicBlock *block = context.create<ir::BasicBlock>("entry");
  if (auto stmt = dynamic_cast<StmtAST *>(blockAST->stmt.get())) {
    if (auto op = convertStmt(context, stmt)) {
      block->insert_back(op);
    }
  }
  return block;
}

ir::Function *convertASTToIR(ir::IRContext &context,
                             std::unique_ptr<ast::BaseAST> &ast) {
  CompUnitAST *compUnit = dynamic_cast<CompUnitAST *>(ast.get());
  FuncDefAST *funcDef =
      compUnit ? dynamic_cast<FuncDefAST *>(compUnit->funcDef.get()) : nullptr;
  FuncTypeAST *funcTypeAST =
      funcDef ? dynamic_cast<FuncTypeAST *>(funcDef->funcType.get()) : nullptr;
  auto funcType =
      funcTypeAST ? convertFunctionType(context, funcTypeAST) : nullptr;

  if (!funcDef || !funcType)
    return nullptr;

  ir::Function *function =
      context.create<ir::Function>(funcDef->ident, funcType);
  if (auto entryBlock = convertBlock(
          context, dynamic_cast<BlockAST *>(funcDef->block.get()))) {
    function->addBlock(entryBlock);
  }

  return function;
}

} // namespace conversion
