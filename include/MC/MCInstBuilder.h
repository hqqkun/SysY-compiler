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
    inst.getOperands().push_back(MCOperand::createImm(imm));
    return *this;
  }

  MCInstBuilder &addReg(const MCRegister &reg) {
    inst.getOperands().push_back(MCOperand::createReg(reg));
    return *this;
  }

  operator MCInst &() { return inst; }

private:
  MCInst inst;
};
} // namespace mc

#endif // __MC_MCINSTBUILDER_H__
