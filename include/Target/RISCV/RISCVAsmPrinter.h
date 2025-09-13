#ifndef __TARGET_RISCV_RISCVASMPRINTER_H__
#define __TARGET_RISCV_RISCVASMPRINTER_H__

#include <vector>

#include "CodeGen/AsmPrinter.h"
#include "IR/Function.h"

namespace target {
namespace riscv {

class RISCVAsmPrinter : public codegen::AsmPrinter {
public:
  explicit RISCVAsmPrinter(std::ostream &os) : AsmPrinter(os) {}
  void emitAsmHeader(const ir::Function *func) override;
  void emitFunction(const ir::Function *func,
                    const std::vector<mc::MCInst> &instrs);
  void emitInstructions(const std::vector<mc::MCInst> &instrs) override;
};

} // namespace riscv
} // namespace target

#endif // __TARGET_RISCV_RISCVASMPRINTER_H__
