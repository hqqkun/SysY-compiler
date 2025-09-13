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
}

void RISCVAsmPrinter::emitInstructions(const std::vector<mc::MCInst> &instrs) {
  for (const mc::MCInst &instr : instrs) {
    switch (instr.getOpType()) {
    case riscv::LI:
      // TODO: make the verify before this function.
      assert(instr.getNumOperands() == 2 &&
             "LI instruction must have 2 operands");
      assert(instr.getOperand(0).isReg() && "First operand must be a register");
      assert(instr.getOperand(1).isImm() &&
             "Second operand must be an immediate");
      out << "\tli "
          << getRegisterName(
                 static_cast<Register>(instr.getOperand(0).getReg().id()))
          << ", " << instr.getOperand(1).getImm() << "\n";
      break;
    case riscv::RET:
      out << "\tret\n";
      break;
    }
  }
}

} // namespace riscv
} // namespace target
