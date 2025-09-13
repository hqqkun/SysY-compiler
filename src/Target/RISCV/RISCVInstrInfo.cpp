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
    assert(retVal->isInteger() && "Return value is not an integer");
    int32_t imm = static_cast<Integer *>(retVal)->getValue();
    outInsts.push_back(
        mc::MCInstBuilder(riscv::LI).addReg(riscv::A0).addImm(imm));
  }
  outInsts.push_back(mc::MCInstBuilder(riscv::RET));
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

} // namespace riscv
} // namespace target
