#include <cassert>
#include <vector>

#include "IR/BasicBlock.h"
#include "IR/Function.h"
#include "IR/Operation.h"
#include "MC/MCInst.h"
#include "Target/RISCV/RISCVISel.h"

namespace target {
namespace riscv {

static uint32_t getMaximumStackArgs(const ir::Function *func) {
  if (!func) {
    return 0;
  }
  uint32_t maxStackArgs = 0;
  for (ir::BasicBlock *bb : *func) {
    for (ir::Operation *op : *bb) {
      if (auto *call = dynamic_cast<ir::CallOp *>(op)) {
        uint32_t numArgs = call->getNumOperands();
        if (numArgs > kMaxArgRegisters) {
          maxStackArgs = std::max(maxStackArgs, numArgs - kMaxArgRegisters);
        }
      }
    }
  }
  return maxStackArgs;
}

static bool isNeedStackForReturn(const ir::Function *func) {
  if (!func) {
    return false;
  }
  for (ir::BasicBlock *bb : *func) {
    for (ir::Operation *op : *bb) {
      if (dynamic_cast<ir::CallOp *>(op)) {
        return true;
      }
    }
  }
  return false;
}

std::vector<mc::MCInst>
RISCVISel::selectInstructions(const ir::Function *func) {
  if (!func) {
    return {};
  }

  std::vector<mc::MCInst> mcInsts;
  instrInfo.resetMap();
  // LV8
  // 1. Calculate stack slots maximally needed for function call arguments.
  const uint32_t maxStackArgs = getMaximumStackArgs(func);
  instrInfo.incrementStackOffset(maxStackArgs * wordSize);

  // 2. For each OpResult in the function, allocate a stack slot.
  for (ir::BasicBlock *bb : *func) {
    for (ir::Operation *op : *bb) {
      assert(op && "Operation is null");
      if (op->hasResult()) {
        instrInfo.addStackSlot(op->getResult());
      }
    }
  }

  // 3. Determine if we need stack space for return address.
  if (isNeedStackForReturn(func)) {
    instrInfo.reserveRaStackSlot();
  }

  // 4. Adjust the stack size for the function prologue.
  const uint32_t stackSize = instrInfo.getAlignedStackSize();
  instrInfo.emitPrologue(mcInsts, stackSize);

  // 5. Lower each operation to machine instructions.
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
      } else if (auto *call = dynamic_cast<ir::CallOp *>(op)) {
        instrInfo.lowerCallOp(call, mcInsts);
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
