#include <cassert>
#include <cstdint>
#include <functional>
#include <string_view>
#include <typeindex>
#include <unordered_map>
#include <vector>

#include "IR/Operation.h"
#include "IR/Value.h"
#include "MC/MCInstBuilder.h"
#include "Target/RISCV/RISCVInstrInfo.h"

using namespace ir;

namespace target {
namespace riscv {

/// Get the size of the given type in bytes.
size_t RISCVInstrInfo::getTypeSizeInBytes(ir::Type *type) {
  assert(type && "Type cannot be null");
  if (type->isInteger()) {
    auto intType = static_cast<ir::IntegerType *>(type);
    return intType->getBitWidth() / 8;
  }
  if (type->isPointer()) {
    return riscv::wordSize; // Assume pointer size is word size.
  }

  if (type->isArray()) {
    auto arrayType = static_cast<ir::ArrayType *>(type);
    return getTypeSizeInBytes(arrayType->getElementType()) *
           arrayType->getSize();
  }

  assert(false && "Unsupported type for size calculation");
  return 0;
}

static inline bool isInImm12Range(int32_t imm) {
  return imm >= kMinImm12 && imm <= kMaxImm12;
}

using OpHandler = std::function<void(Register, Register, Register,
                                     std::vector<mc::MCInst> &)>;

/// Map from IR operation type to RISC-V instruction handler.
static const std::unordered_map<std::type_index, OpHandler> opHandlers = {
    {typeid(ir::AddOp),
     [](Register dst, Register lhs, Register rhs, auto &insts) {
       insts.push_back(
           mc::MCInstBuilder(riscv::ADD).addReg(dst).addReg(lhs).addReg(rhs));
     }},
    {typeid(ir::SubOp),
     [](Register dst, Register lhs, Register rhs, auto &insts) {
       insts.push_back(
           mc::MCInstBuilder(riscv::SUB).addReg(dst).addReg(lhs).addReg(rhs));
     }},
    {typeid(ir::MulOp),
     [](Register dst, Register lhs, Register rhs, auto &insts) {
       insts.push_back(
           mc::MCInstBuilder(riscv::MUL).addReg(dst).addReg(lhs).addReg(rhs));
     }},
    {typeid(ir::DivOp),
     [](Register dst, Register lhs, Register rhs, auto &insts) {
       insts.push_back(
           mc::MCInstBuilder(riscv::DIV).addReg(dst).addReg(lhs).addReg(rhs));
     }},
    {typeid(ir::ModOp),
     [](Register dst, Register lhs, Register rhs, auto &insts) {
       insts.push_back(
           mc::MCInstBuilder(riscv::REM).addReg(dst).addReg(lhs).addReg(rhs));
     }},
    {typeid(ir::EqOp),
     [](Register dst, Register lhs, Register rhs, auto &insts) {
       insts.push_back(
           mc::MCInstBuilder(riscv::XOR).addReg(dst).addReg(lhs).addReg(rhs));
       insts.push_back(mc::MCInstBuilder(riscv::SEQZ).addReg(dst).addReg(dst));
     }},
    {typeid(ir::NeqOp),
     [](Register dst, Register lhs, Register rhs, auto &insts) {
       insts.push_back(
           mc::MCInstBuilder(riscv::XOR).addReg(dst).addReg(lhs).addReg(rhs));
       insts.push_back(mc::MCInstBuilder(riscv::SNEZ).addReg(dst).addReg(dst));
     }},
    {typeid(ir::LessOp),
     [](Register dst, Register lhs, Register rhs, auto &insts) {
       insts.push_back(
           mc::MCInstBuilder(riscv::SLT).addReg(dst).addReg(lhs).addReg(rhs));
     }},
    {typeid(ir::LessEqualOp),
     [](Register dst, Register lhs, Register rhs, auto &insts) {
       insts.push_back(
           mc::MCInstBuilder(riscv::SGT).addReg(dst).addReg(lhs).addReg(rhs));
       insts.push_back(mc::MCInstBuilder(riscv::SEQZ).addReg(dst).addReg(dst));
     }},
    {typeid(ir::GreaterOp),
     [](Register dst, Register lhs, Register rhs, auto &insts) {
       insts.push_back(
           mc::MCInstBuilder(riscv::SGT).addReg(dst).addReg(lhs).addReg(rhs));
     }},
    {typeid(ir::GreaterEqualOp),
     [](Register dst, Register lhs, Register rhs, auto &insts) {
       insts.push_back(
           mc::MCInstBuilder(riscv::SLT).addReg(dst).addReg(lhs).addReg(rhs));
       insts.push_back(mc::MCInstBuilder(riscv::SEQZ).addReg(dst).addReg(dst));
     }},
    {typeid(ir::BitAndOp),
     [](Register dst, Register lhs, Register rhs, auto &insts) {
       insts.push_back(
           mc::MCInstBuilder(riscv::AND).addReg(dst).addReg(lhs).addReg(rhs));
     }},
    {typeid(ir::BitOrOp),
     [](Register dst, Register lhs, Register rhs, auto &insts) {
       insts.push_back(
           mc::MCInstBuilder(riscv::OR).addReg(dst).addReg(lhs).addReg(rhs));
     }},
};

/// Compute the effective address for an element pointer: base + index *
/// elemSize.
static void computeEffectiveAddress(Register baseReg, Register indexReg,
                                    Register elemSizeReg, size_t elemSize,
                                    std::vector<mc::MCInst> &outInsts) {
  outInsts.push_back(mc::MCInstBuilder(riscv::LI)
                         .addReg(elemSizeReg)
                         .addImm(static_cast<int32_t>(elemSize)));
  outInsts.push_back(mc::MCInstBuilder(riscv::MUL)
                         .addReg(indexReg)
                         .addReg(indexReg)
                         .addReg(elemSizeReg));
  outInsts.push_back(mc::MCInstBuilder(riscv::ADD)
                         .addReg(baseReg)
                         .addReg(baseReg)
                         .addReg(indexReg));
}

/// Adds an immediate value to the value in a source register and stores the
/// result in a destination register, handling both 12-bit and larger
/// immediates.
static inline void addImmediateToRegister(Register dest, Register src,
                                          int32_t imm,
                                          std::vector<mc::MCInst> &outInsts,
                                          Register temp = riscv::T0) {
  if (isInImm12Range(imm)) {
    // Immediate fits in 12 bits, use ADDI directly.
    outInsts.push_back(
        mc::MCInstBuilder(riscv::ADDI).addReg(dest).addReg(src).addImm(imm));
    return;
  }
  // Immediate does not fit in 12 bits, load it into a temporary register first.
  outInsts.push_back(mc::MCInstBuilder(riscv::LI).addReg(temp).addImm(imm));
  outInsts.push_back(
      mc::MCInstBuilder(riscv::ADD).addReg(dest).addReg(src).addReg(temp));
}

/// Emits RISC-V LW instruction(s) to load data into the destination register
/// from the address computed by base register plus offset, using direct LW if
/// offset fits in 12 bits, otherwise generating offset loading and addition
/// first.
static inline void emitLoadWithOffset(Register dst, Register baseReg,
                                      int32_t offset,
                                      std::vector<mc::MCInst> &outInsts) {
  if (isInImm12Range(offset)) {
    // Immediate fits in 12 bits, use LW directly.
    outInsts.push_back(
        mc::MCInstBuilder(riscv::LW).addReg(dst).addMem(baseReg, offset));
    return;
  }

  assert(dst != baseReg &&
         "Destination register must be different from base register");
  // Immediate does not fit in 12 bits, load it into a temporary register first.
  outInsts.push_back(mc::MCInstBuilder(riscv::LI).addReg(dst).addImm(offset));
  outInsts.push_back(
      mc::MCInstBuilder(riscv::ADD).addReg(dst).addReg(baseReg).addReg(dst));
  outInsts.push_back(mc::MCInstBuilder(riscv::LW).addReg(dst).addMem(dst, 0));
}

/// Emits RISC-V SW instruction(s) to store source register data to address
/// (base + offset), handling large offsets with a temporary register.
/// baseReg is not changed during emission.
static inline void emitStoreWithOffset(Register value, Register baseReg,
                                       int32_t offset,
                                       std::vector<mc::MCInst> &outInsts,
                                       Register temp = riscv::T0) {
  // IMPORTANT: Should not change baseReg.
  if (isInImm12Range(offset)) {
    // Immediate fits in 12 bits, use SW directly.
    outInsts.push_back(
        mc::MCInstBuilder(riscv::SW).addReg(value).addMem(baseReg, offset));
    return;
  }

  assert(temp != baseReg && temp != value &&
         "Temporary register must be different from base and source registers");
  // Immediate does not fit in 12 bits, load it into a temporary register first.
  outInsts.push_back(mc::MCInstBuilder(riscv::LI).addReg(temp).addImm(offset));
  outInsts.push_back(
      mc::MCInstBuilder(riscv::ADD).addReg(temp).addReg(baseReg).addReg(temp));
  outInsts.push_back(
      mc::MCInstBuilder(riscv::SW).addReg(value).addMem(temp, 0));
}

/// Load a value from the stack with a base register and offset into a temporary
/// register.
static Register loadFromStackWithBase(Register dst, Register baseReg,
                                      int32_t offset,
                                      std::vector<mc::MCInst> &outInsts) {
  emitLoadWithOffset(dst, baseReg, offset, outInsts);
  return dst;
}

/// Get the address of a global variable into a temporary register.
static inline void getGlobalVarAddress(Register temp,
                                       const std::string &varName,
                                       std::vector<mc::MCInst> &outInsts) {
  // Load address of the global variable into `temp`.
  outInsts.push_back(
      mc::MCInstBuilder(riscv::LA).addReg(temp).addLabel(varName));
}

/// Load a value from a global variable into a temporary register.
static void loadFromGlobalVar(Register temp, const std::string &varName,
                              std::vector<mc::MCInst> &outInsts) {
  // Load address of the global variable into `temp`.
  getGlobalVarAddress(temp, varName, outInsts);
  // Load the value from the address in `temp` into `temp`.
  emitLoadWithOffset(temp, temp, 0, outInsts);
}

static void storeToGlobalVar(Register src, Register temp,
                             std::vector<mc::MCInst> &outInsts,
                             const std::string &varName) {
  // Load address of the global variable into `temp`.
  outInsts.push_back(
      mc::MCInstBuilder(riscv::LA).addReg(temp).addLabel(varName));
  // Store the value from `src` into the address in `temp`.
  emitStoreWithOffset(src, temp, 0, outInsts);
}

/// Load a pointer from a stack slot, then store a value to the address pointed
/// to by that pointer.
static void storeViaStackPointer(Register srcReg, uint32_t ptrSlot,
                                 Register tempReg,
                                 std::vector<mc::MCInst> &outInsts) {
  // Load the actual address from the stack slot.
  emitLoadWithOffset(tempReg, riscv::SP, ptrSlot, outInsts);
  emitStoreWithOffset(srcReg, tempReg, 0, outInsts);
}

void RISCVInstrInfo::lowerReturn(ReturnOp *retOp,
                                 std::vector<mc::MCInst> &outInsts,
                                 const uint32_t stackSize) {
  assert(retOp && "ReturnOp is null");
  Value *retVal = retOp->getReturnValue();
  if (retVal) {
    // Return with value.
    Register retReg = lowerOperand(retVal, Register::A0, outInsts);
    // If the returned register is ZERO, load immediate 0 into A0.
    if (retReg == riscv::ZERO) {
      outInsts.push_back(
          mc::MCInstBuilder(riscv::LI).addReg(riscv::A0).addImm(0));
    }
  }

  emitEpilogue(outInsts, stackSize);
  outInsts.push_back(mc::MCInstBuilder(riscv::RET));
}

void RISCVInstrInfo::lowerBinaryOp(ir::BinaryOp *binOp,
                                   std::vector<mc::MCInst> &outInsts) {
  assert(binOp && "BinaryOp is null");
  Value *lhs = binOp->getLHS();
  Value *rhs = binOp->getRHS();
  assert(lhs && rhs && "BinaryOp operands cannot be null");

  Register lhsReg = lowerOperand(lhs, Register::T0, outInsts);
  Register rhsReg = lowerOperand(rhs, Register::T1, outInsts);
  Register opDstReg = riscv::UNKNOWN;
  if (lhsReg != riscv::ZERO) {
    opDstReg = lhsReg;
  } else if (rhsReg != riscv::ZERO) {
    opDstReg = rhsReg;
  } else {
    opDstReg = riscv::T0; // Both operands are ZERO, use T0 as destination.
  }

  auto it = opHandlers.find(typeid(*binOp));
  assert(it != opHandlers.end() && "Unsupported BinaryOp type");

  // Emit MC instructions.
  it->second(opDstReg, lhsReg, rhsReg, outInsts);
  uint32_t resultSlot = getStackSlot(binOp->getResult());
  Register tempReg = (opDstReg != riscv::T0) ? riscv::T0 : riscv::T1;
  emitStoreWithOffset(opDstReg, riscv::SP, resultSlot, outInsts, tempReg);
}

void RISCVInstrInfo::lowerLoadOp(ir::LoadOp *loadOp,
                                 std::vector<mc::MCInst> &outInsts) {
  assert(loadOp && "LoadOp is null");
  Value *ptr = loadOp->getPointer();
  Register src = lowerOperand(ptr, Register::T0, outInsts);
  if (isElementPtr(ptr)) {
    emitLoadWithOffset(src, src, 0, outInsts);
  }
  Value *result = loadOp->getResult();
  uint32_t resultSlot = getStackSlot(result);

  // Handle large offset using a temporary register.
  Register temp = (riscv::T0 != src) ? riscv::T0 : riscv::T1;
  emitStoreWithOffset(src, riscv::SP, resultSlot, outInsts, temp);
}

void RISCVInstrInfo::lowerStoreOp(ir::StoreOp *storeOp,
                                  std::vector<mc::MCInst> &outInsts) {
  assert(storeOp && "StoreOp is null");
  Value *val = storeOp->getValue();
  Register valReg = lowerOperand(val, Register::T0, outInsts);

  Value *ptr = storeOp->getPointer();
  Register tempReg = (valReg != riscv::T0) ? riscv::T0 : riscv::T1;
  if (isGlobalVariable(ptr)) {
    const std::string &varName = getGlobalVarName(ptr);
    storeToGlobalVar(valReg, tempReg, outInsts, varName);
  } else {
    uint32_t ptrSlot = getStackSlot(ptr);
    if (isElementPtr(ptr)) {
      storeViaStackPointer(valReg, ptrSlot, tempReg, outInsts);
      return;
    }

    // Handle large offset using a temporary register.
    emitStoreWithOffset(valReg, riscv::SP, ptrSlot, outInsts, tempReg);
  }
}

void RISCVInstrInfo::lowerBranchOp(ir::CondBranchOp *brOp,
                                   std::vector<mc::MCInst> &outInsts) {
  assert(brOp && "BranchOp is null");
  Value *cond = brOp->getCondition();
  ir::BasicBlock *thenBB = brOp->getThenBB();
  ir::BasicBlock *elseBB = brOp->getElseBB();
  assert(cond && thenBB && elseBB && "BranchOp operands cannot be null");

  Register condReg = lowerOperand(cond, Register::T0, outInsts);
  // BNEZ condReg, thenBB
  outInsts.push_back(mc::MCInstBuilder(riscv::BNEZ)
                         .addReg(condReg)
                         .addLabel(thenBB->getName()));
  // JUMP elseBB
  outInsts.push_back(
      mc::MCInstBuilder(riscv::JUMP).addLabel(elseBB->getName()));
}

void RISCVInstrInfo::lowerJumpOp(ir::JumpOp *jumpOp,
                                 std::vector<mc::MCInst> &outInsts) {
  assert(jumpOp && "JumpOp is null");
  ir::BasicBlock *targetBB = jumpOp->getTargetBB();
  assert(targetBB && "JumpOp target BasicBlock cannot be null");

  // JUMP targetBB
  outInsts.push_back(
      mc::MCInstBuilder(riscv::JUMP).addLabel(targetBB->getName()));
}

void RISCVInstrInfo::lowerCallOp(ir::CallOp *callOp,
                                 std::vector<mc::MCInst> &outInsts) {
  assert(callOp && "CallOp is null");

  // First 8 arguments are passed in A0-A7.
  const size_t numArgs = callOp->getNumOperands();
  for (size_t i = 0; i < numArgs && i < kMaxArgRegisters; ++i) {
    Value *arg = callOp->getOperand(i);
    assert(arg && "CallOp argument cannot be null");
    Register argReg =
        lowerOperand(arg, static_cast<Register>(riscv::A0 + i), outInsts);
    // If the argument register is ZERO, load immediate 0 into the argument
    // register.
    if (argReg == riscv::ZERO) {
      outInsts.push_back(mc::MCInstBuilder(riscv::LI)
                             .addReg(static_cast<Register>(riscv::A0 + i))
                             .addImm(0));
    }
  }

  // Handle additional arguments on the stack.
  for (size_t i = kMaxArgRegisters; i < numArgs; ++i) {
    Value *arg = callOp->getOperand(i);
    assert(arg && "CallOp argument cannot be null");
    Register tempReg = lowerOperand(arg, Register::T0, outInsts);
    uint32_t argOffset = (i - kMaxArgRegisters) * wordSize;
    Register tempForStore = (tempReg != riscv::T0) ? riscv::T0 : riscv::T1;
    emitStoreWithOffset(tempReg, riscv::SP, argOffset, outInsts, tempForStore);
  }

  // CALL function
  outInsts.push_back(
      mc::MCInstBuilder(riscv::CALL).addLabel(callOp->getFunctionName()));

  // If the call has a return value, store it to the stack slot.
  if (callOp->hasResult()) {
    Value *result = callOp->getResult();
    uint32_t resultSlot = getStackSlot(result);
    emitStoreWithOffset(riscv::A0, riscv::SP, resultSlot, outInsts);
  }
}

/// Lower the given GetElemPtrOp to RISC-V instructions.
void RISCVInstrInfo::lowerGetElemPtrOp(ir::GetElemPtrOp *gepOp,
                                       std::vector<mc::MCInst> &outInsts) {
  assert(gepOp && "GetElemPtrOp is null");
  ir::Type *elemType = gepOp->getElementType();
  ir::Value *base = gepOp->getBasePointer();
  ir::Value *index = gepOp->getIndex();
  uint32_t resultSlot = getStackSlot(gepOp->getResult());
  markAsElementPtr(gepOp->getResult());

  // Lower base pointer.
  Register baseReg = riscv::T0;
  emitAddressForValue(baseReg, base, outInsts);

  // Lower index.
  Register indexReg = lowerOperand(index, Register::T1, outInsts);
  if (indexReg == riscv::ZERO) {
    // If index is ZERO, the effective address is just the base address.
    Register temp = (baseReg != riscv::T0) ? riscv::T0 : riscv::T1;
    emitStoreWithOffset(baseReg, riscv::SP, resultSlot, outInsts, temp);
    return;
  }

  // Compute effective address: base + index * elemSize
  size_t elemSize = getTypeSizeInBytes(elemType);
  Register elemSizeReg = riscv::T2;
  computeEffectiveAddress(baseReg, indexReg, elemSizeReg, elemSize, outInsts);
  emitStoreWithOffset(baseReg, riscv::SP, resultSlot, outInsts, indexReg);
}

/// Lower the given GetPtrOp to RISC-V instructions.
void RISCVInstrInfo::lowerGetPtrOp(ir::GetPtrOp *getPtrOp,
                                   std::vector<mc::MCInst> &outInsts) {
  assert(getPtrOp && "GetPtrOp is null");
  ir::Type *pointeeType = getPtrOp->getPointeeType();
  ir::Value *base = getPtrOp->getPointer();
  ir::Value *index = getPtrOp->getIndex();
  uint32_t resultSlot = getStackSlot(getPtrOp->getResult());
  markAsElementPtr(getPtrOp->getResult());

  // Load base pointer from stack slot.
  Register baseReg = riscv::T0;
  uint32_t stackSlot = getStackSlot(base);
  emitLoadWithOffset(baseReg, riscv::SP, stackSlot, outInsts);

  // Lower index.
  Register indexReg = lowerOperand(index, Register::T1, outInsts);
  if (indexReg == riscv::ZERO) {
    // If index is ZERO, the effective address is just the base address.
    Register temp = (baseReg != riscv::T0) ? riscv::T0 : riscv::T1;
    emitStoreWithOffset(baseReg, riscv::SP, resultSlot, outInsts, temp);
    return;
  }

  // Compute effective address: base + index * pointeeSize
  size_t pointeeSize = getTypeSizeInBytes(pointeeType);
  Register elemSizeReg = riscv::T2;
  computeEffectiveAddress(baseReg, indexReg, elemSizeReg, pointeeSize,
                          outInsts);
  emitStoreWithOffset(baseReg, riscv::SP, resultSlot, outInsts, indexReg);
}

void RISCVInstrInfo::emitAddressForValue(Register baseReg, ir::Value *base,
                                         std::vector<mc::MCInst> &outInsts) {
  // Handle global variable: directly get its address (e.g., via symbol
  // reference).
  if (isGlobalVariable(base)) {
    const std::string &varName = getGlobalVarName(base);
    getGlobalVarAddress(baseReg, varName, outInsts);
    return;
  }

  const uint32_t stackSlot = getStackSlot(base);
  if (isElementPtr(base)) {
    // Case 1: Base is an element pointer (already stored in stack slot)
    // Load the pre-computed address from the stack slot into baseReg.
    emitLoadWithOffset(baseReg, riscv::SP, stackSlot, outInsts);
  } else {
    // Case 2: Direct stack object (compute address as SP + stackSlot)
    // Calculate address by adding stack slot offset to stack pointer (SP).
    addImmediateToRegister(baseReg, riscv::SP, static_cast<int32_t>(stackSlot),
                           outInsts);
  }
}

/// Lower the given IR value to a given RISC-V register.
Register RISCVInstrInfo::lowerOperand(ir::Value *val, Register temp,
                                      std::vector<mc::MCInst> &outInsts) {
  assert(val && "Value is null");

  if (val->isInteger()) {
    int imm = static_cast<Integer *>(val)->getValue();
    // `0` is a special immediate value that can be directly mapped to the ZERO
    // register.
    if (imm == 0) {
      return riscv::ZERO;
    }
    // LI temp, imm
    outInsts.push_back(mc::MCInstBuilder(riscv::LI).addReg(temp).addImm(imm));
    return temp;
  }

  // Check if the value is a global variable.
  if (isGlobalVariable(val)) {
    const std::string &varName = getGlobalVarName(val);
    loadFromGlobalVar(temp, varName, outInsts);
    return temp;
  }

  // Check if the value is a function argument.
  if (val->isFuncArg()) {
    auto arg = static_cast<ir::FuncArg *>(val);
    size_t index = arg->getIndex();
    if (index < kMaxArgRegisters) {
      return static_cast<Register>(riscv::A0 + index);
    }
    // Argument is passed on the stack.
    uint32_t offset = (index - kMaxArgRegisters) * wordSize;
    return loadFromStackWithBase(temp, riscv::FP, offset, outInsts);
  }

  return loadFromStackWithBase(temp, riscv::SP, getStackSlot(val), outInsts);
}

bool RISCVInstrInfo::isNeedStackForRa(const ir::Function *func) {
  assert(func && "Function is null");
  for (ir::BasicBlock *bb : *func) {
    for (ir::Operation *op : *bb) {
      if (dynamic_cast<ir::CallOp *>(op)) {
        return true;
      }
    }
  }
  return false;
}

void RISCVInstrInfo::reserveRaAndFpStackSlots(const ir::Function *func) {
  assert(func && "Function is null");

  // Reserve stack slot for return address if needed.
  if (isNeedStackForRa(func)) {
    assert(!raStackOffset.has_value() &&
           "Return address stack offset is already set");
    raStackOffset = offset;
    offset += wordSize;
  }

  if (func->getNumArgs() > kMaxArgRegisters) {
    assert(!fpStackOffset.has_value() &&
           "Frame pointer stack offset is already set");
    fpStackOffset = offset;
    offset += wordSize;
  }
}

void RISCVInstrInfo::emitPrologue(std::vector<mc::MCInst> &outInsts,
                                  const uint32_t stackSize) {
  if (stackSize > 0) {
    // Adjust stack pointer: addi sp, sp, -stackSize
    addImmediateToRegister(riscv::SP, riscv::SP,
                           -static_cast<int32_t>(stackSize), outInsts);
    // Push return address and frame pointer if needed.
    if (raStackOffset.has_value()) {
      emitStoreWithOffset(riscv::RA, riscv::SP, *raStackOffset, outInsts);
    }
    // Push frame pointer if needed.
    if (fpStackOffset.has_value()) {
      emitStoreWithOffset(riscv::FP, riscv::SP, *fpStackOffset, outInsts);
      // Set frame pointer to current stack pointer.
      // addi fp, sp, stackSize
      addImmediateToRegister(riscv::FP, riscv::SP,
                             static_cast<int32_t>(stackSize), outInsts);
    }
  }
}

void RISCVInstrInfo::emitEpilogue(std::vector<mc::MCInst> &outInsts,
                                  const uint32_t stackSize) {
  if (stackSize > 0) {
    // Pop return address.
    if (raStackOffset.has_value()) {
      emitLoadWithOffset(riscv::RA, riscv::SP, *raStackOffset, outInsts);
    }
    // Pop frame pointer.
    if (fpStackOffset.has_value()) {
      emitLoadWithOffset(riscv::FP, riscv::SP, *fpStackOffset, outInsts);
    }
    // Restore stack pointer: addi sp, sp, stackSize
    addImmediateToRegister(riscv::SP, riscv::SP,
                           static_cast<int32_t>(stackSize), outInsts);
  }
}

void RISCVInstrInfo::emitLabel(std::string_view label,
                               std::vector<mc::MCInst> &outInsts) {
  outInsts.push_back(mc::MCInstBuilder(riscv::LABEL).addLabel(label));
}

std::string_view getRegisterName(Register reg) {
  static std::unordered_map<Register, std::string_view> regNames = {
      {ZERO, "zero"}, {RA, "ra"}, {SP, "sp"},   {GP, "gp"},   {TP, "tp"},
      {T0, "t0"},     {T1, "t1"}, {T2, "t2"},   {S0, "s0"},   {FP, "fp"},
      {S1, "s1"},     {A0, "a0"}, {A1, "a1"},   {A2, "a2"},   {A3, "a3"},
      {A4, "a4"},     {A5, "a5"}, {A6, "a6"},   {A7, "a7"},   {S2, "s2"},
      {S3, "s3"},     {S4, "s4"}, {S5, "s5"},   {S6, "s6"},   {S7, "s7"},
      {S8, "s8"},     {S9, "s9"}, {S10, "s10"}, {S11, "s11"}, {T3, "t3"},
      {T4, "t4"},     {T5, "t5"}, {T6, "t6"}};

  if (auto it = regNames.find(reg); it != regNames.end()) {
    return it->second;
  }
  assert(false && "Unknown register");
}

std::string_view getOpTypeName(OpType op) {
  static const std::unordered_map<OpType, std::string_view> opNames = {
      {ADD, "add"},   {ADDI, "addi"}, {AND, "and"},   {BNEZ, "bnez"},
      {CALL, "call"}, {DIV, "div"},   {JUMP, "j"},    {LA, "la"},
      {LI, "li"},     {LW, "lw"},     {MUL, "mul"},   {MV, "mv"},
      {OR, "or"},     {REM, "rem"},   {RET, "ret"},   {SEQZ, "seqz"},
      {SGT, "sgt"},   {SLT, "slt"},   {SNEZ, "snez"}, {SUB, "sub"},
      {SW, "sw"},     {XOR, "xor"}};

  if (auto it = opNames.find(op); it != opNames.end()) {
    return it->second;
  }
  assert(false && "Unknown OpType");
}

} // namespace riscv
} // namespace target
