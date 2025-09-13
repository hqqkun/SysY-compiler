#include <cassert>
#include <vector>

#include "IR/BasicBlock.h"
#include "IR/Function.h"
#include "IR/Operation.h"
#include "MC/MCInst.h"
#include "Target/RISCV/RISCVISel.h"

namespace target {
namespace riscv {

std::vector<mc::MCInst>
RISCVISel::selectInstructions(const ir::Function *func) {
  if (!func) {
    return {};
  }

  std::vector<mc::MCInst> mcInsts;
  for (ir::BasicBlock *bb : *func) {
    for (ir::Operation *op : *bb) {
      assert(op && "Operation is null");
      if (auto retOp = dynamic_cast<ir::ReturnOp *>(op)) {
        instrInfo.lowerReturn(retOp, mcInsts);
      } else {
        // Handle other operation types here.
        // For now, we can just ignore them or throw an error.
        throw std::runtime_error(
            "Unsupported operation type in instruction selection");
      }
    }
  }
  return mcInsts;
}

} // namespace riscv
} // namespace target
