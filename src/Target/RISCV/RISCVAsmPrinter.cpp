#include <cassert>
#include <vector>

#include "CodeGen/AsmPrinter.h"
#include "IR/Function.h"
#include "Target/RISCV/RISCVAsmPrinter.h"
#include "Target/RISCV/RISCVInstrInfo.h"

namespace target {
namespace riscv {

void RISCVAsmPrinter::emitAsmHeader(const ir::Function *func) {
  assert(func && "Function is null");
  out << "\t.text\n";
  // Assume the function is global for simplicity.
  out << "\t.globl " << func->getName() << "\n";
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

} // namespace riscv
} // namespace target
