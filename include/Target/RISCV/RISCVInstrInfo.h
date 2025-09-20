#ifndef __TARGET_RISCV_RISCVINSTRINFO_H__
#define __TARGET_RISCV_RISCVINSTRINFO_H__

#include <string_view>
#include <unordered_map>
#include <vector>

#include "IR/Operation.h"
#include "MC/MCInst.h"

namespace target {
namespace riscv {

enum OpType {
  ADD,
  AND,
  DIV,
  LI, // Load Immediate
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
  void lowerReturn(ir::ReturnOp *retOp, std::vector<mc::MCInst> &outInsts);
  void lowerBinaryOp(ir::BinaryOp *binOp, std::vector<mc::MCInst> &outInsts);
  void resetRegMap() { value2RegMap.clear(); }

private:
  std::unordered_map<ir::Value *, Register> value2RegMap;
  Register lowerOperand(ir::Value *val, std::vector<mc::MCInst> &outInsts);
};

std::string_view getRegisterName(Register reg);
std::string_view getOpTypeName(OpType op);

} // namespace riscv
} // namespace target

#endif // __TARGET_RISCV_RISCVINSTRINFO_H__
