#ifndef __CODEGEN_ASMPRINTER_H__
#define __CODEGEN_ASMPRINTER_H__

#include <ostream>
#include <vector>

#include "IR/Function.h"
#include "MC/MCInst.h"

namespace codegen {

class AsmPrinter {
public:
  explicit AsmPrinter(std::ostream &os) : out(os) {}

  virtual ~AsmPrinter() = default;
  //! Only support `1` function now.
  virtual void emitAsmHeader(const ir::Function *func) = 0;
  virtual void emitInstructions(const std::vector<mc::MCInst> &instrs) = 0;
  virtual void emitLabel(std::string label) { out << label << ":\n"; }

protected:
  std::ostream &out;
};

} // namespace codegen

#endif // __CODEGEN_ASMPRINTER_H__
