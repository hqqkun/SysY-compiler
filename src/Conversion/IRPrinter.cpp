#include <cassert>
#include <cstddef>
#include <ostream>
#include <unordered_map>

#include "Conversion/IRPrinter.h"
#include "IR/BasicBlock.h"
#include "IR/Function.h"
#include "IR/Operation.h"

namespace conversion {

static void printAllocPrefix(std::ostream &os, bool isUserVariable) {
  if (isUserVariable) {
    os << "@";
  } else {
    os << "%";
  }
}

void IRPrinter::printFunction(ir::Function *func) {
  assert(func && "Function cannot be null");

  os << "fun @" << func->getName() << " ";
  printType(func->getFunctionType());
  os << " {" << std::endl;
  OpResultMap resultMap;
  for (ir::BasicBlock *bb : *func) {
    if (bb->size()) {
      printBasicBlock(bb, resultMap);
    }
  }
  os << "}" << std::endl;
}

void IRPrinter::printBasicBlock(ir::BasicBlock *block, OpResultMap &resultMap) {
  assert(block && "BasicBlock cannot be null");
  // Print block label.
  os << "%" << block->getName() << ":" << std::endl;
  for (ir::Operation *op : *block) {
    printOperation(op, resultMap);
  }
}

void IRPrinter::printOperation(ir::Operation *op, OpResultMap &resultMap) {
  assert(op && "Operation cannot be null");

  os << "\t"; // Create indent.

  // Handle different operation types for printing
  if (auto *allocOp = dynamic_cast<ir::AllocOp *>(op)) {
    printAllocOperation(allocOp, resultMap);
    return;
  }
  if (auto *brOp = dynamic_cast<ir::BranchOp *>(op)) {
    printBranchOperation(brOp, resultMap);
    return;
  }

  // Print format: %id = op operands | op operands
  if (op->hasResult()) {
    // If the operation produces a result, assign an ID and print it.
    ir::OpResult *result = op->getResult();
    assert(result && "Operation's result is null");
    uint64_t id = resultMap.insert(result);
    os << "%" << id << " = ";
  }

  // Print operation name.
  os << op->getOpName() << " ";

  // Print operands.
  for (size_t i = 0; i < op->getNumOperands(); ++i) {
    if (i > 0) {
      os << ", ";
    }
    printOperand(op->getOperand(i), resultMap);
  }
  os << std::endl;
}

/// This is a specialized function to print BranchOp.
void IRPrinter::printBranchOperation(ir::BranchOp *brOp,
                                     OpResultMap &resultMap) {
  assert(brOp && "BranchOp cannot be null");
  os << brOp->getOpName() << " ";
  if (auto *condBr = dynamic_cast<ir::CondBranchOp *>(brOp)) {
    printOperand(condBr->getCondition(), resultMap);
    os << ", %" << condBr->getThenBB()->getName() << ", %"
       << condBr->getElseBB()->getName();
  } else if (auto *jumpOp = dynamic_cast<ir::JumpOp *>(brOp)) {
    os << "%" << jumpOp->getTargetBB()->getName();
  } else {
    assert(false && "Unknown BranchOp type");
  }
  os << std::endl;
}

/// Specialized function to print AllocOp with variable names.
void IRPrinter::printAllocOperation(ir::AllocOp *allocOp,
                                    OpResultMap &resultMap) {
  assert(allocOp && "AllocOp cannot be null");
  bool isUserVar = allocOp->isUserVariable();
  printAllocPrefix(os, isUserVar);
  if (isUserVar) {
    // @varName = alloc type
    allocNames[allocOp->getResult()] = allocOp->getVarName();
    os << allocOp->getVarName();
  } else {
    // For non-user variables, assign a unique name using the result ID.
    // %id = alloc type
    uint64_t id = resultMap.insert(allocOp->getResult());
    os << id;
  }
  os << " = " << allocOp->getOpName() << " ";
  printType(allocOp->getAllocType());
  os << std::endl;
}

void IRPrinter::printOperand(ir::Value *operand, OpResultMap &resultMap) {
  assert(operand && "Operand cannot be null");
  if (operand->isInteger()) {
    ir::Integer *intVal = static_cast<ir::Integer *>(operand);
    os << intVal->getValue();
  } else if (operand->isOpResult()) {
    ir::OpResult *opRes = static_cast<ir::OpResult *>(operand);

    // Check if it's from an AllocOp to print variable name.
    if (allocNames.find(opRes) != allocNames.end()) {
      os << "@" << allocNames[opRes];
      return;
    }
    uint64_t id = resultMap.getId(opRes);
    os << "%" << id;
  } else if (operand->isFuncArg()) {
    ir::FuncArg *funcArg = static_cast<ir::FuncArg *>(operand);
    os << "arg" << funcArg->getIndex();
  } else {
    assert(false && "Unknown operand type");
  }
}

void IRPrinter::printType(ir::Type *type) {
  assert(type && "Type cannot be null");
  if (type->isInteger()) {
    ir::IntegerType *intType = static_cast<ir::IntegerType *>(type);
    os << "i" << intType->getBitWidth();
  } else if (type->isVoid()) {
    os << "void";
  } else if (type->isFunction()) {
    ir::FunctionType *funcType = static_cast<ir::FunctionType *>(type);
    os << "(";
    const auto &paramTypes = funcType->getParamTypes();
    for (size_t i = 0; i < paramTypes.size(); ++i) {
      if (i > 0) {
        os << ", ";
      }
      printType(paramTypes[i]);
    }
    os << ")";
    if (!funcType->hasReturnType()) {
      return;
    }
    os << ": ";
    printType(funcType->getReturnType());
  } else {
    assert(false && "Unknown type");
  }
}

} // namespace conversion
