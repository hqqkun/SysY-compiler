#ifndef __TARGET_RISCV_RISCVINSTRINFO_H__
#define __TARGET_RISCV_RISCVINSTRINFO_H__

#include <cassert>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "IR/Operation.h"
#include "IR/Value.h"
#include "MC/MCInst.h"

namespace target {
namespace riscv {

constexpr uint32_t wordSize = 4;         // 4 bytes for 32-bit RISC-V.
constexpr uint32_t kMaxArgRegisters = 8; // a0 - a7

enum OpType {
  ADD,
  ADDI,
  AND,
  BNEZ, // Branch if Not Equal to Zero
  CALL, // Call a function
  DIV,
  JUMP,
  LABEL, // Label
  LI,    // Load Immediate
  LW,    // Load Word
  MUL,
  MV,
  OR,
  REM,
  RET, // Return
  SEQZ,
  SGT,
  SLT,
  SNEZ,
  SUB,
  SW, // Store Word
  XOR,
  // TODO: Add more RISC-V instruction op types as needed.
};

enum Register {
  UNKNOWN = -1,
  ZERO = 0, // Hard-wired zero
  RA = 1,   // Return address
  SP = 2,   // Stack pointer
  GP = 3,   // Global pointer
  TP = 4,   // Thread pointer
  T0 = 5,   // Temporary registers
  T1 = 6,
  T2 = 7,
  S0 = 8, // Saved registers
  FP = 8, // Frame pointer (alias for S0)
  S1 = 9,
  A0 = 10, // Function arguments / return values
  A1 = 11,
  A2 = 12,
  A3 = 13,
  A4 = 14,
  A5 = 15,
  A6 = 16,
  A7 = 17,
  S2 = 18, // More saved registers
  S3 = 19,
  S4 = 20,
  S5 = 21,
  S6 = 22,
  S7 = 23,
  S8 = 24,
  S9 = 25,
  S10 = 26,
  S11 = 27,
  T3 = 28, // More temporary registers
  T4 = 29,
  T5 = 30,
  T6 = 31
};

class RISCVInstrInfo {
public:
  void lowerReturn(ir::ReturnOp *retOp, std::vector<mc::MCInst> &outInsts,
                   const uint32_t stackSize = 0);
  void lowerBinaryOp(ir::BinaryOp *binOp, std::vector<mc::MCInst> &outInsts);
  void lowerLoadOp(ir::LoadOp *loadOp, std::vector<mc::MCInst> &outInsts);
  void lowerStoreOp(ir::StoreOp *storeOp, std::vector<mc::MCInst> &outInsts);
  void lowerBranchOp(ir::CondBranchOp *brOp, std::vector<mc::MCInst> &outInsts);
  void lowerJumpOp(ir::JumpOp *jumpOp, std::vector<mc::MCInst> &outInsts);
  void lowerCallOp(ir::CallOp *callOp, std::vector<mc::MCInst> &outInsts);
  void emitPrologue(std::vector<mc::MCInst> &outInsts,
                    const uint32_t stackSize = 0);
  void emitLabel(std::string_view label, std::vector<mc::MCInst> &outInsts);

  void resetMap() {
    value2RegMap.clear();
    value2StackSlotMap.clear();
    raStackOffset.reset();
    offset = 0;
  }

  /// LV4: Assign a stack slot to the given value if it doesn't have one.
  void addStackSlot(ir::Value *val) {
    if (value2StackSlotMap.find(val) == value2StackSlotMap.end()) {
      value2StackSlotMap[val] = offset;
      offset += wordSize;
    }
  }

  void reserveRaStackSlot() {
    assert(!raStackOffset.has_value() &&
           "Return address stack offset is already set");
    raStackOffset = offset;
    offset += wordSize;
  }

  void incrementStackOffset(uint32_t size) { offset += size; }

  uint32_t getStackSlot(ir::Value *val) {
    auto it = value2StackSlotMap.find(val);
    if (it != value2StackSlotMap.end()) {
      return it->second;
    }
    assert(false && "Value does not have an assigned stack slot");
  }

  uint32_t getAlignedStackSize(const uint32_t align = 16) const {
    // Align the stack size to 16 bytes for RISC-V calling convention.
    return ((offset + align - 1) / align) * align;
  }

private:
  std::unordered_map<ir::Value *, Register> value2RegMap;
  Register lowerOperand(ir::Value *val, Register temp,
                        std::vector<mc::MCInst> &outInsts);
  void emitEpilogue(std::vector<mc::MCInst> &outInsts,
                    const uint32_t stackSize);
  void pushRa(std::vector<mc::MCInst> &outInsts);
  void popRa(std::vector<mc::MCInst> &outInsts);

  // LV4: map each OpResult to a stack slot.
  std::unordered_map<ir::Value *, uint32_t> value2StackSlotMap;
  uint32_t offset = 0;                   // Current stack offset in bytes.
  std::optional<uint32_t> raStackOffset; // Stack offset for return address.
};

std::string_view getRegisterName(Register reg);
std::string_view getOpTypeName(OpType op);

} // namespace riscv
} // namespace target

#endif // __TARGET_RISCV_RISCVINSTRINFO_H__
