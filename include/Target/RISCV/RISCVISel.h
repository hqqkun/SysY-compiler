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

private:
  RISCVInstrInfo instrInfo;
};

} // namespace riscv
} // namespace target

#endif // __TARGET_RISCV_RISCVISEL_H__
