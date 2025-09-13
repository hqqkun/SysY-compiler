#include "Target/RISCV/RISCVTargetMachine.h"

namespace target {
namespace riscv {
void RISCVTargetMachine::codeGen(const ir::Function *func) {
  if (!func) {
    out << "# No function to generate code for.\n";
    return;
  }
  // Instruction Selection.
  std::vector<mc::MCInst> instrs = isel.selectInstructions(func);
  // Emit Assembly.
  asmPrinter.emitFunction(func, instrs);
}

} // namespace riscv
} // namespace target
