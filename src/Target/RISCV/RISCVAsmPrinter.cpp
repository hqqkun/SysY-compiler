#include <cassert>
#include <string_view>
#include <vector>

#include "CodeGen/AsmPrinter.h"
#include "IR/Function.h"
#include "Target/RISCV/RISCVAsmPrinter.h"
#include "Target/RISCV/RISCVInstrInfo.h"

namespace target {
namespace riscv {

constexpr std::string_view kDataSection = ".data";
constexpr std::string_view kTextSection = ".text";
constexpr std::string_view kGlobalDirective = ".globl";
constexpr std::string_view kWordDirective = ".word";
constexpr std::string_view kZeroInit = ".zero";

void RISCVAsmPrinter::emitAsmHeader(const ir::Function *func) {
  assert(func && "Function is null");
  out << "\t" << kTextSection << std::endl;
  // Assume the function is global for simplicity.
  out << "\t" << kGlobalDirective << " " << func->getName() << std::endl;
}

void RISCVAsmPrinter::emitFunction(const ir::Function *func,
                                   const std::vector<mc::MCInst> &instrs) {
  emitAsmHeader(func);
  emitLabel(func->getName());
  emitInstructions(instrs);
  out << "\n";
}

void RISCVAsmPrinter::emitInstructions(const std::vector<mc::MCInst> &instrs) {
  for (const mc::MCInst &instr : instrs) {
    auto opType = static_cast<OpType>(instr.getOpType());

    // Handle labels separately.
    if (opType == LABEL) {
      assert(instr.getNumOperands() == 1 &&
             "LABEL instruction must have exactly one operand");
      const mc::MCOperand &op = instr.getOperand(0);
      assert(op.isLabel() && "LABEL operand must be a label");
      out << op.getLabel().getLabel() << ":\n";
      continue;
    }
    out << "\t" << getOpTypeName(opType) << "\t";
    emitOperands(instr.getOperands());
    out << "\n";
  }
}

void RISCVAsmPrinter::emitOperands(const std::vector<mc::MCOperand> &operands) {
  for (size_t i = 0; i < operands.size(); ++i) {
    if (i > 0) {
      out << ", ";
    }
    const mc::MCOperand &op = operands[i];
    if (op.isReg()) {
      out << getRegisterName(static_cast<Register>(op.getReg().id()));
    } else if (op.isImm()) {
      out << op.getImm();
    } else if (op.isMem()) {
      mc::MCMemory mem = op.getMem();
      out << mem.getOffset() << "("
          << getRegisterName(static_cast<Register>(mem.getBaseReg().id()))
          << ")";
    } else if (op.isLabel()) {
      out << op.getLabel().getLabel();
    } else {
      assert(false && "Unsupported operand type");
    }
  }
}

void RISCVAsmPrinter::emitGlobalVarDecl(const ir::GlobalVarDecl *varDecl) {
  assert(varDecl && "GlobalVarDecl is null");
  ir::GlobalAlloc *alloc = varDecl->getAllocation();
  assert(alloc && "GlobalAlloc is null");

  const std::string &varName = varDecl->getIdent();
  out << "\t" << kDataSection << std::endl;
  out << "\t" << kGlobalDirective << " " << varName << std::endl;
  emitLabel(varName);

  if (alloc->isSingle()) {
    // Print initial value if exists.
    ir::Value *initValue = alloc->getInitValue();
    if (initValue) {
      assert(initValue->getType()->isInteger() &&
             "Only integer initial value is supported");
      ir::Integer *intVal = static_cast<ir::Integer *>(initValue);
      out << "\t" << kWordDirective << " " << intVal->getValue() << std::endl;
    } else {
      out << "\t" << kZeroInit << " " << wordSize << std::endl;
    }
    out << std::endl;
    return;
  }

  if (alloc->isMultiple()) {
    const auto &initValues = alloc->getInitValues();
    auto *arrayType = dynamic_cast<ir::ArrayType *>(alloc->getElementType());
    assert(arrayType && "Multi-value initializer must be array type");
    size_t totalSize = RISCVInstrInfo::getTypeSizeInBytes(arrayType);
    if (initValues.empty()) {
      out << "\t" << kZeroInit << " " << totalSize << std::endl;
      out << std::endl;
      return;
    }
    size_t arraySize = arrayType->getSize();
    // Print all provided initial values.
    size_t printedCount = 0;
    for (ir::Value *init : initValues) {
      assert(init->getType()->isInteger() &&
             "Only integer initial values are supported");
      ir::Integer *intVal = static_cast<ir::Integer *>(init);
      out << "\t" << kWordDirective << " " << intVal->getValue() << std::endl;
      printedCount++;
    }
    // Zero-initialize the remaining elements.
    for (size_t i = printedCount; i < arraySize; ++i) {
      out << "\t" << kWordDirective << " 0" << std::endl;
    }
  }

  out << std::endl;
}

} // namespace riscv
} // namespace target
