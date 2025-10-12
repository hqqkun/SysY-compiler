#ifndef __TARGET_RISCV_RISCVISEL_H__
#define __TARGET_RISCV_RISCVISEL_H__

#include <vector>

#include "IR/Function.h"
#include "MC/MCInst.h"
#include "Target/RISCV/RISCVInstrInfo.h"

namespace target {
namespace riscv {

class RISCVISel {
public:
  explicit RISCVISel() = default;
  std::vector<mc::MCInst> selectInstructions(const ir::Function *func);
  // LV8: Add a global variable to the set.
  /// This is a workaround since RISCVISel does not have a
  /// dedicated pass to process global variables.
  void addGlobalVariable(ir::Value *val, const std::string &name) {
    instrInfo.addGlobalVariable(val, name);
  }

private:
  RISCVInstrInfo instrInfo;
};

} // namespace riscv
} // namespace target

#endif // __TARGET_RISCV_RISCVISEL_H__
