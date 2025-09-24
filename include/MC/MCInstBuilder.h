#ifndef __MC_MCINSTBUILDER_H__
#define __MC_MCINSTBUILDER_H__

/// For building MCInst objects conveniently.

#include "MC/MCInst.h"

namespace mc {
class MCInstBuilder {

public:
  explicit MCInstBuilder() = delete;
  explicit MCInstBuilder(unsigned opType) { inst.setOpType(opType); }

  MCInstBuilder &addImm(int32_t imm) {
    inst.addOperand(MCOperand::createImm(imm));
    return *this;
  }

  MCInstBuilder &addReg(const MCRegister &reg) {
    inst.addOperand(MCOperand::createReg(reg));
    return *this;
  }

  MCInstBuilder &addMem(uint64_t baseReg, uint32_t offset) {
    inst.addOperand(MCOperand::createMem(baseReg, offset));
    return *this;
  }

  MCInstBuilder &addLabel(std::string_view label) {
    inst.addOperand(MCOperand::createLabel(label));
    return *this;
  }

  operator MCInst &() { return inst; }

private:
  MCInst inst;
};
} // namespace mc

#endif // __MC_MCINSTBUILDER_H__
