#include <cassert>
#include <cstdint>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "IR/Operation.h"
#include "IR/Value.h"
#include "MC/MCInstBuilder.h"
#include "Target/RISCV/RISCVInstrInfo.h"

using namespace ir;

namespace target {
namespace riscv {

/// naive implementation of register allocation.
Register allocateNewRegister() {
  static const std::vector<Register> availableRegs = {
      A0, A1, A2, A3, A4, A5, A6, A7, T0, T1, T2, T3, T4, T5, T6};
  static size_t nextRegIdx = 0;
  assert(nextRegIdx < availableRegs.size() &&
         "Ran out of registers for allocation");
  return availableRegs[nextRegIdx++];
}

void RISCVInstrInfo::lowerReturn(ReturnOp *retOp,
                                 std::vector<mc::MCInst> &outInsts) {
  assert(retOp && "ReturnOp is null");
  Value *retVal = retOp->getReturnValue();
  if (!retVal) {
    // Return without value, default to returning 0.
    outInsts.push_back(
        mc::MCInstBuilder(riscv::LI).addReg(riscv::A0).addImm(0));
  } else {
    // Return with value.
    Register retReg = lowerOperand(retVal, outInsts);
    outInsts.push_back(
        mc::MCInstBuilder(riscv::MV).addReg(riscv::A0).addReg(retReg));
  }
  outInsts.push_back(mc::MCInstBuilder(riscv::RET));
}

void RISCVInstrInfo::lowerBinaryOp(ir::BinaryOp *binOp,
                                   std::vector<mc::MCInst> &outInsts) {
  assert(binOp && "BinaryOp is null");
  Value *lhs = binOp->getLHS();
  Value *rhs = binOp->getRHS();
  assert(lhs && rhs && "BinaryOp operands cannot be null");
  assert(value2RegMap.find(binOp->getResult()) == value2RegMap.end() &&
         "Result is already allocated a register");

  Register lhsReg = lowerOperand(lhs, outInsts);
  Register rhsReg = lowerOperand(rhs, outInsts);
  Register dstReg = allocateNewRegister();
  if (auto *sub = dynamic_cast<ir::SubOp *>(binOp)) {
    (void)sub;
    outInsts.push_back(mc::MCInstBuilder(riscv::SUB)
                           .addReg(dstReg)
                           .addReg(lhsReg)
                           .addReg(rhsReg));
  } else if (auto *eq = dynamic_cast<ir::EqOp *>(binOp)) {
    (void)eq;
    outInsts.push_back(mc::MCInstBuilder(riscv::XOR)
                           .addReg(dstReg)
                           .addReg(lhsReg)
                           .addReg(rhsReg));
    outInsts.push_back(
        mc::MCInstBuilder(riscv::SEQZ).addReg(dstReg).addReg(dstReg));
  }
  value2RegMap[binOp->getResult()] = dstReg;
}

Register RISCVInstrInfo::lowerOperand(ir::Value *val,
                                      std::vector<mc::MCInst> &outInsts) {
  assert(val && "Value is null");
  // If the value is already allocated a register, reuse it.
  if (!val->isInteger()) {
    auto it = value2RegMap.find(val);
    assert(it != value2RegMap.end() && "Value not found in register map");
    return it->second;
  }

  int imm = static_cast<Integer *>(val)->getValue();
  assert(value2RegMap.find(val) == value2RegMap.end() &&
         "Value is already allocated a register");
  if (imm == 0) {
    // `0` is a special immediate value that can be directly mapped to the ZERO
    // register.
    value2RegMap.insert({val, riscv::ZERO});
    return riscv::ZERO;
  }
  // LI dst, imm
  Register dst = allocateNewRegister();
  value2RegMap.insert({val, dst});
  outInsts.push_back(mc::MCInstBuilder(riscv::LI).addReg(dst).addImm(imm));
  return dst;
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
  static std::unordered_map<OpType, std::string_view> opNames = {
      {LI, "li"},   {RET, "ret"},   {SUB, "sub"},
      {XOR, "xor"}, {SEQZ, "seqz"}, {MV, "mv"}};
  if (auto it = opNames.find(op); it != opNames.end()) {
    return it->second;
  }
  assert(false && "Unknown OpType");
}

} // namespace riscv
} // namespace target
