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

void RISCVInstrInfo::lowerReturn(ReturnOp *retOp,
                                 std::vector<mc::MCInst> &outInsts,
                                 const uint32_t stackSize) {
  assert(retOp && "ReturnOp is null");
  Value *retVal = retOp->getReturnValue();
  if (!retVal) {
    // Return without value, default to returning 0.
    outInsts.push_back(
        mc::MCInstBuilder(riscv::LI).addReg(riscv::A0).addImm(0));
  } else {
    // Return with value.
    Register retReg = lowerOperand(retVal, Register::A0, outInsts);
    // Small optimization: if the retReg is not A0, move it to A0.
    if (retReg != riscv::A0) {
      outInsts.push_back(
          mc::MCInstBuilder(riscv::MV).addReg(riscv::A0).addReg(retReg));
    }
  }

  if (stackSize != 0) {
    // Restore stack pointer: addi sp, sp, stackSize
    outInsts.push_back(mc::MCInstBuilder(riscv::ADDI)
                           .addReg(riscv::SP)
                           .addReg(riscv::SP)
                           .addImm(static_cast<int32_t>(stackSize)));
  }
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
  outInsts.push_back(mc::MCInstBuilder(riscv::SW).addReg(opDstReg).addMem(
      riscv::SP, resultSlot));
}

void RISCVInstrInfo::lowerLoadOp(ir::LoadOp *loadOp,
                                 std::vector<mc::MCInst> &outInsts) {
  assert(loadOp && "LoadOp is null");
  Value *ptr = loadOp->getPointer();
  Value *result = loadOp->getResult();
  uint32_t offsetSlot = getStackSlot(ptr);
  uint32_t resultSlot = getStackSlot(result);
  outInsts.push_back(mc::MCInstBuilder(riscv::LW)
                         .addReg(Register::T0)
                         .addMem(riscv::SP, offsetSlot));
  outInsts.push_back(mc::MCInstBuilder(riscv::SW)
                         .addReg(Register::T0)
                         .addMem(riscv::SP, resultSlot));
}

void RISCVInstrInfo::lowerStoreOp(ir::StoreOp *storeOp,
                                  std::vector<mc::MCInst> &outInsts) {
  assert(storeOp && "StoreOp is null");
  Value *val = storeOp->getValue();
  Value *ptr = storeOp->getPointer();
  uint32_t ptrSlot = getStackSlot(ptr);
  Register valReg = lowerOperand(val, Register::T0, outInsts);
  outInsts.push_back(
      mc::MCInstBuilder(riscv::SW).addReg(valReg).addMem(riscv::SP, ptrSlot));
}

Register RISCVInstrInfo::lowerOperand(ir::Value *val, Register temp,
                                      std::vector<mc::MCInst> &outInsts) {
  assert(val && "Value is null");
  if (!val->isInteger()) {
    uint32_t offset = getStackSlot(val);
    outInsts.push_back(
        mc::MCInstBuilder(riscv::LW).addReg(temp).addMem(riscv::SP, offset));
    return temp;
  }

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

void RISCVInstrInfo::emitPrologue(std::vector<mc::MCInst> &outInsts,
                                  const uint32_t stackSize) {
  if (stackSize > 0) {
    // Adjust stack pointer: addi sp, sp, -stackSize
    outInsts.push_back(mc::MCInstBuilder(riscv::ADDI)
                           .addReg(riscv::SP)
                           .addReg(riscv::SP)
                           .addImm(-static_cast<int32_t>(stackSize)));
  }
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
      {ADD, "add"}, {ADDI, "addi"}, {AND, "and"}, {DIV, "div"}, {LI, "li"},
      {LW, "lw"},   {MUL, "mul"},   {MV, "mv"},   {OR, "or"},   {REM, "rem"},
      {RET, "ret"}, {SEQZ, "seqz"}, {SGT, "sgt"}, {SLT, "slt"}, {SNEZ, "snez"},
      {SUB, "sub"}, {SW, "sw"},     {XOR, "xor"}};

  if (auto it = opNames.find(op); it != opNames.end()) {
    return it->second;
  }
  assert(false && "Unknown OpType");
}

} // namespace riscv
} // namespace target
