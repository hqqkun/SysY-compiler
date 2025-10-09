#ifndef __CONVERSION_IRPRINTER_H__
#define __CONVERSION_IRPRINTER_H__

#include <cassert>
#include <ostream>
#include <string>
#include <unordered_map>

#include "IR/BasicBlock.h"
#include "IR/Function.h"
#include "IR/Module.h"
#include "IR/Operation.h"

namespace conversion {

class IRPrinter {
public:
  explicit IRPrinter(std::ostream &o) : os(o) {}

  void printModule(ir::Module *module);

private:
  std::ostream &os;
  using ResultIdMap = std::unordered_map<ir::OpResult *, uint64_t>;
  std::unordered_map<ir::OpResult *, std::string> allocNames;

  struct OpResultMap {
    explicit OpResultMap() = default;
    ResultIdMap result2Id;
    uint64_t counter = 0;

    uint64_t getId(ir::OpResult *result) {
      auto it = result2Id.find(result);
      assert(it != result2Id.end() && "OpResult not found in map");
      return it->second;
    }

    uint64_t insert(ir::OpResult *result) {
      auto [it, inserted] = result2Id.emplace(result, counter);
      if (inserted) {
        return counter++;
      } else {
        return it->second;
      }
    }
  };

  void printFunction(ir::Function *func);
  void printBasicBlock(ir::BasicBlock *block, OpResultMap &resultMap);
  void printOperation(ir::Operation *op, OpResultMap &resultMap);
  void printAllocOperation(ir::AllocOp *allocOp, OpResultMap &resultMap);
  void printCallOperation(ir::CallOp *callOp, OpResultMap &resultMap);
  void printBranchOperation(ir::BranchOp *brOp, OpResultMap &resultMap);
  void printOperand(ir::Value *operand, OpResultMap &resultMap);
  void printBasicType(ir::Type *type);
  void printFunctionType(ir::FunctionType *funcType,
                         const std::vector<std::string> &paramNames,
                         bool withParamNames = true);
  void printFunctionDeclaration(ir::FunctionDecl *funcDecl);
  void reset() { allocNames.clear(); }
};

} // namespace conversion

#endif // __CONVERSION_IRPRINTER_H__
