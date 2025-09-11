#include <ostream>
#include <string_view>

#include "IR/BasicBlock.h"

namespace ir {
static constexpr std::string_view kPrefix = "%";

void BasicBlock::print(std::ostream &os) const {
  os << kPrefix << name << ":" << std::endl;
  for (Operation *op : operations) {
    op->print(os);
  }
}
} // namespace ir
