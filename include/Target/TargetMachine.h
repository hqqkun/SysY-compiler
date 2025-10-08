#ifndef __TARGET_TARGETMACHINE_H__
#define __TARGET_TARGETMACHINE_H__

#include <ostream>

#include "IR/Module.h"

namespace target {

class TargetMachine {
public:
  explicit TargetMachine(std::ostream &o) : out(o) {}
  virtual ~TargetMachine() = default;

  // Generate target-specific assembly code
  // for the given module to the output stream.
  virtual void codeGen(const ir::Module *module) = 0;

protected:
  std::ostream &out;
};

} // namespace target

#endif // __TARGET_TARGETMACHINE_H__
