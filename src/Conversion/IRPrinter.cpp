#include <cassert>
#include <cstddef>
#include <functional>
#include <numeric>
#include <ostream>
#include <unordered_map>

#include "Conversion/IRPrinter.h"
#include "IR/BasicBlock.h"
#include "IR/Function.h"
#include "IR/Operation.h"
#include "Utils/Utils.h"

namespace conversion {

constexpr std::string_view kZeroInitStr = "zeroinit";

static void printAllocPrefix(std::ostream &os, bool isUserVariable) {
  if (isUserVariable) {
    os << "@";
  } else {
    os << "%";
  }
}

void IRPrinter::printModule(ir::Module *module) {
  assert(module && "Module cannot be null");
  // Print all function declarations first.
  for (ir::Declaration *decl : module->getDeclarations()) {
    assert(decl && "Declaration cannot be null");
    if (decl->isFunction()) {
      auto *funcDecl = static_cast<ir::FunctionDecl *>(decl);
      printFunctionDeclaration(funcDecl);
    } else if (decl->isGlobalVar()) {
      auto *varDecl = static_cast<ir::GlobalVarDecl *>(decl);
      printGlobalVarDeclaration(varDecl);
    }
  }
  if (module->getDeclarations().size()) {
    os << std::endl; // Separate declarations and definitions with a newline.
  }

  for (ir::Function *func : *module) {
    printFunction(func);
    os << std::endl; // Separate functions with a newline.
    reset();
  }
}

void IRPrinter::printFunction(ir::Function *func) {
  assert(func && "Function cannot be null");

  // Collect argument names.
  std::vector<std::string> argNames;
  for (ir::FuncArg *arg : func->getArgs()) {
    argNames.push_back(arg->getName());
  }

  os << "fun @" << func->getName();
  printFunctionType(func->getFunctionType(), argNames);
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
  if (auto *allocOp = dynamic_cast<ir::LocalAlloc *>(op)) {
    printAllocOperation(allocOp, resultMap);
    return;
  }
  if (auto *brOp = dynamic_cast<ir::BranchOp *>(op)) {
    printBranchOperation(brOp, resultMap);
    return;
  }

  if (auto *call = dynamic_cast<ir::CallOp *>(op)) {
    printCallOperation(call, resultMap);
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

/// This is a specialized function to print CallOp.
void IRPrinter::printCallOperation(ir::CallOp *callOp, OpResultMap &resultMap) {
  assert(callOp && "CallOp cannot be null");
  if (callOp->hasResult()) {
    ir::OpResult *result = callOp->getResult();
    assert(result && "CallOp's result is null");
    uint64_t id = resultMap.insert(result);
    os << "%" << id << " = ";
  }
  os << callOp->getOpName() << " @" << callOp->getFunctionName();
  os << "(";
  // Print operands.
  for (size_t i = 0; i < callOp->getNumOperands(); ++i) {
    if (i > 0) {
      os << ", ";
    }
    printOperand(callOp->getOperand(i), resultMap);
  }
  os << ")" << std::endl;
}

/// Specialized function to print AllocOp with variable names.
void IRPrinter::printAllocOperation(ir::LocalAlloc *allocOp,
                                    OpResultMap &resultMap) {
  assert(allocOp && "AllocOp cannot be null");
  bool isUserVar = allocOp->isUserVariable();
  printAllocPrefix(os, isUserVar);
  if (isUserVar) {
    // @varName = alloc type
    AddAllocName(allocOp->getResult(), allocOp->getVarName());
    os << allocOp->getVarName();
  } else {
    // For non-user variables, assign a unique name using the result ID.
    // %id = alloc type
    uint64_t id = resultMap.insert(allocOp->getResult());
    os << id;
  }
  os << " = " << allocOp->getOpName() << " ";
  printBasicType(allocOp->getElementType());
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
    std::optional<std::string> allocName = getAllocName(opRes);
    if (allocName.has_value()) {
      os << "@" << allocName.value();
      return;
    }
    uint64_t id = resultMap.getId(opRes);
    os << "%" << id;
  } else if (operand->isFuncArg()) {
    ir::FuncArg *funcArg = static_cast<ir::FuncArg *>(operand);
    os << "@" << funcArg->getName();
  } else {
    assert(false && "Unknown operand type");
  }
}

void IRPrinter::printFunctionType(ir::FunctionType *funcType,
                                  const std::vector<std::string> &paramNames,
                                  bool withParamNames) {
  assert(funcType && "FunctionType cannot be null");
  if (withParamNames) {
    assert(funcType->getParamTypes().size() == paramNames.size() &&
           "Parameter names size must match parameter types size");
  }
  os << "(";
  const auto &paramTypes = funcType->getParamTypes();
  for (size_t i = 0; i < paramTypes.size(); ++i) {
    if (i > 0) {
      os << ", ";
    }
    if (withParamNames) {
      os << "@" << paramNames[i] << ": ";
    }
    printBasicType(paramTypes[i]);
  }
  os << ")";
  if (funcType->hasReturnType()) {
    os << ": ";
    printBasicType(funcType->getReturnType());
  }
}

/// Print basic types like integer, void and pointer types.
void IRPrinter::printBasicType(ir::Type *type) {
  assert(type && "Type cannot be null");
  if (type->isInteger()) {
    ir::IntegerType *intType = static_cast<ir::IntegerType *>(type);
    os << "i" << intType->getBitWidth();
  } else if (type->isVoid()) {
    os << "void";
  } else if (type->isPointer()) {
    ir::PointerType *ptrType = static_cast<ir::PointerType *>(type);
    os << "*";
    printBasicType(ptrType->getPointeeType());
  } else if (type->isArray()) {
    ir::ArrayType *arrayType = static_cast<ir::ArrayType *>(type);
    os << "[";
    printBasicType(arrayType->getElementType());
    os << ", " << arrayType->getSize() << "]";
  } else {
    assert(false && "Unknown type");
  }
}

void IRPrinter::printFunctionDeclaration(ir::FunctionDecl *funcDecl) {
  assert(funcDecl && "FunctionDecl cannot be null");

  os << "decl @" << funcDecl->getIdent();
  printFunctionType(funcDecl->getFunctionType(), {}, false);
  os << std::endl;
}

void IRPrinter::printGlobalVarDeclaration(ir::GlobalVarDecl *varDecl) {
  assert(varDecl && "GlobalVarDecl cannot be null");
  ir::GlobalAlloc *allocOp = varDecl->getAllocation();
  assert(allocOp && "GlobalVarDecl's allocation cannot be null");

  // IMPORTANT: Map the allocation result to the variable name for later use.
  // Global variables must be user variables.
  AddAllocName(allocOp->getResult(), varDecl->getIdent(), true);

  // global @var = alloc i32, (init_value | zeroinit)
  os << "global @" << varDecl->getIdent() << " = " << allocOp->getOpName()
     << " ";
  printBasicType(allocOp->getElementType());
  os << ", ";

  OpResultMap dummyMap; // Dummy map for printing the init value.
  if (allocOp->isSingle()) {
    printSingleInitializer(allocOp, dummyMap);
  } else {
    printArrayInitializer(allocOp, dummyMap);
  }
  os << std::endl;
}

void IRPrinter::printSingleInitializer(ir::GlobalAlloc *allocOp,
                                       OpResultMap &map) {
  assert(allocOp && "GlobalAlloc cannot be null");

  ir::Value *initValue = allocOp->getInitValue();
  if (initValue) {
    printOperand(initValue, map);
  } else {
    os << kZeroInitStr;
  }
}

/// Prints the initializer for a multi-dimensional array in nested braces.
void IRPrinter::printArrayInitializer(ir::GlobalAlloc *allocOp,
                                      OpResultMap &map) {
  assert(allocOp && "GlobalAlloc cannot be null");
  const auto &initValues = allocOp->getInitValues();
  auto *arrayType = dynamic_cast<ir::ArrayType *>(allocOp->getElementType());
  assert(arrayType && "Multi-value initializer must be array type");

  if (initValues.empty()) {
    os << kZeroInitStr;
    return;
  }

  const auto dims = getArrayDimensions(arrayType);
  const size_t totalElements =
      std::accumulate(dims.begin(), dims.end(), 1u, std::multiplies<size_t>());
  assert(initValues.size() == totalElements &&
         "Initializer size mismatch with array dimensions");

  // Recursive lambda to print nested dimensions
  // Parameters: current dimension index, start index in initValues, stride
  // between elements.
  const auto printDimension = [&](auto &self, size_t dimIdx, size_t startIdx,
                                  size_t stride) {
    // Last dimension: directly print elements in sequence
    if (dimIdx == dims.size() - 1) {
      for (size_t i = 0; i < dims[dimIdx]; ++i) {
        if (i != 0)
          os << ", ";
        printOperand(initValues[startIdx + i], map);
      }
      return;
    }

    // Nested dimensions: print sub-dimensions in braces.
    const size_t nextStride = stride / dims[dimIdx + 1];
    for (size_t i = 0; i < dims[dimIdx]; ++i) {
      if (i != 0)
        os << ", ";
      os << "{";
      self(self, dimIdx + 1, startIdx + i * stride, nextStride);
      os << "}";
    }
  };

  // Print outermost braces and start recursion.
  os << "{";
  printDimension(printDimension, 0, 0, totalElements / dims[0]);
  os << "}";
}

void IRPrinter::AddAllocName(ir::OpResult *result, const std::string &name,
                             bool isGlobal) {
  assert(result && "OpResult cannot be null");

  auto &allocNames = isGlobal ? globalAllocNames : LocalallocNames;

  assert(allocNames.find(result) == allocNames.end() &&
         "Variable already has a name assigned");
  allocNames[result] = name;
}

std::optional<std::string> IRPrinter::getAllocName(ir::OpResult *result) const {
  assert(result && "OpResult cannot be null");

  // First check local alloc names, then global alloc names.
  // As the local scope shadows the global scope.
  auto it = LocalallocNames.find(result);
  if (it != LocalallocNames.end()) {
    return it->second;
  }
  it = globalAllocNames.find(result);
  if (it != globalAllocNames.end()) {
    return it->second;
  }
  return std::nullopt;
}

} // namespace conversion
