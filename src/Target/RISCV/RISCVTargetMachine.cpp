#include "Target/RISCV/RISCVTargetMachine.h"

namespace target {
namespace riscv {
void RISCVTargetMachine::codeGen(const ir::Module *module) {
  if (!module) {
    out << "# No module to generate code for.\n";
    return;
  }

  // Emit global variable declarations.
  for (ir::Declaration *decl : module->getDeclarations()) {
    if (decl->isGlobalVar()) {
      auto *varDecl = static_cast<ir::GlobalVarDecl *>(decl);
      asmPrinter.emitGlobalVarDecl(varDecl);
      isel.addGlobalVariable(varDecl->getAllocation()->getResult(),
                             varDecl->getIdent());
    }
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
