#ifndef __TARGET_RISCV_RISCVINSTRINFO_H__
#define __TARGET_RISCV_RISCVINSTRINFO_H__

#include <cassert>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "IR/Function.h"
#include "IR/Operation.h"
#include "IR/Value.h"
#include "MC/MCInst.h"

namespace target {
namespace riscv {

constexpr uint32_t wordSize = 4;         // 4 bytes for 32-bit RISC-V.
constexpr uint32_t kMaxArgRegisters = 8; // a0 - a7
constexpr int32_t kMinImm12 = -2048;
constexpr int32_t kMaxImm12 = 2047;

enum OpType {
  ADD,
  ADDI,
  AND,
  BNEZ, // Branch if Not Equal to Zero
  CALL, // Call a function
  DIV,
  JUMP,
  LA,    // Load Address
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
  void lowerGetElemPtrOp(ir::GetElemPtrOp *gepOp,
                         std::vector<mc::MCInst> &outInsts);
  void emitPrologue(std::vector<mc::MCInst> &outInsts,
                    const uint32_t stackSize = 0);
  void emitLabel(std::string_view label, std::vector<mc::MCInst> &outInsts);

  // LV8: Add a global variable to the set.
  void addGlobalVariable(ir::Value *val, const std::string &name) {
    assert(val && "Global variable value cannot be null");
    assert(globalVarNames.find(val) == globalVarNames.end() &&
           "Global variable already exists");
    globalVarNames[val] = name;
  }

  bool isGlobalVariable(ir::Value *val) const {
    assert(val && "Value cannot be null");
    return globalVarNames.find(val) != globalVarNames.end();
  }

  const std::string &getGlobalVarName(ir::Value *val) const {
    assert(val && "Value cannot be null");
    auto it = globalVarNames.find(val);
    assert(it != globalVarNames.end() && "Global variable not found");
    return it->second;
  }

  /// Reset the internal state before processing a new function.
  /// Note: globalVarNames is not cleared here because global variables
  /// persist across functions and are program-scoped.
  void resetState() {
    value2StackSlotMap.clear();
    elementPtrs.clear();
    raStackOffset.reset();
    fpStackOffset.reset();
    offset = 0;
  }

  /// LV4: Assign a stack slot to the given value if it doesn't have one.
  void addStackSlot(ir::Value *val, uint32_t size = wordSize) {
    if (value2StackSlotMap.find(val) == value2StackSlotMap.end()) {
      value2StackSlotMap[val] = offset;
      offset += size;
    }
  }

  void reserveRaAndFpStackSlots(const ir::Function *func);

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

  void markAsElementPtr(ir::Value *elemPtr) {
    assert(elemPtr && "Element pointer value cannot be null");
    elementPtrs.insert(elemPtr);
  }

  bool isElementPtr(ir::Value *val) const {
    assert(val && "Value cannot be null");
    return elementPtrs.find(val) != elementPtrs.end();
  }

  static size_t getTypeSizeInBytes(ir::Type *type);

private:
  Register lowerOperand(ir::Value *val, Register temp,
                        std::vector<mc::MCInst> &outInsts);
  void emitEpilogue(std::vector<mc::MCInst> &outInsts,
                    const uint32_t stackSize);
  bool isNeedStackForRa(const ir::Function *func);

  // LV4: map each OpResult to a stack slot.
  std::unordered_map<ir::Value *, uint32_t> value2StackSlotMap;
  // LV8: global variables.
  /// Since global variables are not function-scoped, we need to
  /// track them separately. Before lowering any functions, we need to
  /// ensure that all global variables are inserted into the map.
  std::unordered_map<ir::Value *, std::string> globalVarNames;
  uint32_t offset = 0;                   // Current stack offset in bytes.
  std::optional<uint32_t> raStackOffset; // Stack offset for return address.
  std::optional<uint32_t> fpStackOffset; // Stack offset for frame pointer.

  // LV9.
  /// A set of values that are results of GetElementPtr operations (i.e.,
  /// pointers to array elements). These values represent addresses of specific
  /// elements within an array, rather than direct variable addresses. When
  /// handling store or load operations where the pointer operand is in this
  /// set:
  /// - For stores: We need to first load the actual element address from the
  /// stack (since the value itself is stored in a stack slot),
  ///   then perform the store to that address.
  /// - For loads: Similarly, we need to load the element address from the stack
  /// first, then load the value from that address.
  std::unordered_set<ir::Value *> elementPtrs;
};

std::string_view getRegisterName(Register reg);
std::string_view getOpTypeName(OpType op);

} // namespace riscv
} // namespace target

#endif // __TARGET_RISCV_RISCVINSTRINFO_H__
