#include "Target/RISCV/RISCVTargetMachine.h"

namespace target {
namespace riscv {
void RISCVTargetMachine::codeGen(const ir::Module *module) {
  if (!module) {
    out << "# No module to generate code for.\n";
    return;
  }

  for (const ir::Function *func : *module) {
    if (!func) {
      continue;
    }
    // Instruction Selection.
    std::vector<mc::MCInst> instrs = isel.selectInstructions(func);
    // Emit Assembly.
    asmPrinter.emitFunction(func, instrs);
  }
}

} // namespace riscv
} // namespace target
