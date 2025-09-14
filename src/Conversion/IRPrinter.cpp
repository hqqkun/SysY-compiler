
#include <cassert>
#include <cstddef>
#include <ostream>
#include <unordered_map>

#include "Conversion/IRPrinter.h"
#include "IR/BasicBlock.h"
#include "IR/Function.h"
#include "IR/Operation.h"

namespace conversion {

void IRPrinter::printFunction(ir::Function *func) {
  assert(func && "Function cannot be null");
  assert(func->size() == 1 && "Only single-block functions are supported");

  os << "fun @" << func->getName() << " ";
  printType(func->getFunctionType());
  os << " {" << std::endl;
  OpResultMap resultMap;
  for (ir::BasicBlock *bb : *func) {
    printBasicBlock(bb, resultMap);
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

void IRPrinter::printOperand(ir::Value *operand, OpResultMap &resultMap) {
  assert(operand && "Operand cannot be null");
  if (operand->isInteger()) {
    ir::Integer *intVal = static_cast<ir::Integer *>(operand);
    os << intVal->getValue();
  } else if (operand->isOpResult()) {
    ir::OpResult *opRes = static_cast<ir::OpResult *>(operand);
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
      ;
    }
    os << ": ";
    printType(funcType->getReturnType());
  } else {
    assert(false && "Unknown type");
  }
}

} // namespace conversion
