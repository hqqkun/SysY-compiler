#ifndef __TARGET_RISCV_RISCVTARGETMACHINE_H__
#define __TARGET_RISCV_RISCVTARGETMACHINE_H__

#include <ostream>

#include "Target/RISCV/RISCVAsmPrinter.h"
#include "Target/RISCV/RISCVISel.h"
#include "Target/TargetMachine.h"

class RISCVAsmPrinter;

namespace target {
namespace riscv {

class RISCVTargetMachine : public TargetMachine {
public:
  explicit RISCVTargetMachine(std::ostream &o)
      : TargetMachine(o), asmPrinter(o) {}

  void codeGen(const ir::Module *module) override;

private:
  RISCVISel isel;
  RISCVAsmPrinter asmPrinter;
};

} // namespace riscv
} // namespace target

#endif // __TARGET_RISCV_RISCVTARGETMACHINE_H__
