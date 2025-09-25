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
  instrInfo.resetMap();

  // 1. For each OpResult in the function, allocate a stack slot.
  for (ir::BasicBlock *bb : *func) {
    for (ir::Operation *op : *bb) {
      assert(op && "Operation is null");
      if (op->hasResult()) {
        instrInfo.addStackSlot(op->getResult());
      }
    }
  }

  // 2. Adjust the stack size for the function prologue.
  const uint32_t stackSize = instrInfo.getAlignedStackSize();
  instrInfo.emitPrologue(mcInsts, stackSize);

  // 3. Lower each operation to machine instructions.
  for (ir::BasicBlock *bb : *func) {
    instrInfo.emitLabel(bb->getName(), mcInsts);
    for (ir::Operation *op : *bb) {
      if (auto *retOp = dynamic_cast<ir::ReturnOp *>(op)) {
        instrInfo.lowerReturn(retOp, mcInsts, stackSize);
      } else if (auto *binOp = dynamic_cast<ir::BinaryOp *>(op)) {
        instrInfo.lowerBinaryOp(binOp, mcInsts);
      } else if (auto *load = dynamic_cast<ir::LoadOp *>(op)) {
        instrInfo.lowerLoadOp(load, mcInsts);
      } else if (auto *store = dynamic_cast<ir::StoreOp *>(op)) {
        instrInfo.lowerStoreOp(store, mcInsts);
      } else if (auto *alloc = dynamic_cast<ir::AllocOp *>(op)) {
        // AllocOp does not generate any instructions directly.
        // Stack slot is already allocated in the first pass.
        (void)alloc; // Suppress unused variable warning.
      } else if (auto *branch = dynamic_cast<ir::CondBranchOp *>(op)) {
        instrInfo.lowerBranchOp(branch, mcInsts);
      } else if (auto *jump = dynamic_cast<ir::JumpOp *>(op)) {
        instrInfo.lowerJumpOp(jump, mcInsts);
      } else {
        // Handle other operation types here.
        // For now, we can just ignore them or throw an error.
        assert(false && "Unsupported operation type");
      }
    }
  }
  return mcInsts;
}

} // namespace riscv
} // namespace target
