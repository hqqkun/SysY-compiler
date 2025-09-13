#include "Conversion/Conversion.h"
#include "IR/Function.h"
#include "IR/IRBuilder.h"
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

static ir::Operation *convertStmt(ir::IRBuilder &builder, StmtAST *stmtAST) {
  return stmtAST ? builder.create<ir::ReturnOp>(
                       ir::Integer::get(builder.getContext(), stmtAST->number))
                 : nullptr;
}

static ir::BasicBlock *convertBlock(ir::IRBuilder &builder,
                                    BlockAST *blockAST) {
  if (!blockAST)
    return nullptr;

  ir::BasicBlock *block = builder.create<ir::BasicBlock>("entry");
  builder.setInsertPoint(block);
  if (auto stmt = dynamic_cast<StmtAST *>(blockAST->stmt.get())) {
    convertStmt(builder, stmt);
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

  ir::IRBuilder builder(context);
  ir::Function *function =
      builder.create<ir::Function>(funcDef->ident, funcType);
  if (auto entryBlock = convertBlock(
          builder, dynamic_cast<BlockAST *>(funcDef->block.get()))) {
    function->addBlock(entryBlock);
  }

  return function;
}

} // namespace conversion
