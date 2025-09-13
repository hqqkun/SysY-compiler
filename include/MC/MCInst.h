#ifndef __MC_MCINST_H__
#define __MC_MCINST_H__

#include <cassert>
#include <cstdint>
#include <variant>
#include <vector>

// This file is a part of the MC layer for machine code representation.
namespace mc {

class MCRegister {
public:
  constexpr MCRegister(uint64_t reg) : regNum(reg) {}
  constexpr uint64_t id() const { return regNum; }

private:
  uint64_t regNum;
};

class MCOperand {
  enum class MachineOperandType {
    kInvalid,
    kRegister,
    KImmediate, // Immediate integer value.
  };

  using OperandValue = std::variant<uint64_t, // RegVal
                                    int32_t   // ImmVal
                                    >;

public:
  explicit MCOperand() : value(std::in_place_type<int32_t>, 0) {}
  bool isValid() const { return kind != MachineOperandType::kInvalid; }
  bool isReg() const { return kind == MachineOperandType::kRegister; }
  bool isImm() const { return kind == MachineOperandType::KImmediate; }

  int32_t getImm() const {
    assert(isImm() && "This is not an immediate");
    return std::get<int32_t>(value);
  }

  void setImm(int32_t val) {
    assert(isImm() && "This is not an immediate");
    value = val;
  }

  MCRegister getReg() const {
    assert(isReg() && "This is not a register");
    return MCRegister(std::get<uint64_t>(value));
  }

  void setReg(const MCRegister &reg) {
    assert(isReg() && "This is not a register");
    value = reg.id();
  }

  static MCOperand createImm(int32_t val) {
    MCOperand op;
    op.kind = MachineOperandType::KImmediate;
    op.value = val;
    return op;
  }

  static MCOperand createReg(const MCRegister &reg) {
    MCOperand op;
    op.kind = MachineOperandType::kRegister;
    op.value = reg.id();
    return op;
  }

private:
  MachineOperandType kind = MachineOperandType::kInvalid;
  OperandValue value;
};

class MCInst {
public:
  explicit MCInst() = default;
  void setOpType(uint32_t opt) { opType = opt; }
  uint32_t getOpType() const { return opType; }

  const MCOperand &getOperand(size_t idx) const {
    assert(idx < operands.size() && "Operand index out of range");
    return operands[idx];
  }
  MCOperand &getOperand(size_t idx) {
    assert(idx < operands.size() && "Operand index out of range");
    return operands[idx];
  }
  size_t getNumOperands() const { return operands.size(); }

  std::vector<MCOperand> &getOperands() { return operands; }
  void addOperand(const MCOperand &op) { operands.push_back(op); }

  using iterator = std::vector<MCOperand>::iterator;
  using const_iterator = std::vector<MCOperand>::const_iterator;

  void clear() { operands.clear(); }
  void erase(iterator I) { operands.erase(I); }
  void erase(iterator first, iterator last) { operands.erase(first, last); }
  size_t size() const { return operands.size(); }
  iterator begin() { return operands.begin(); }
  const_iterator begin() const { return operands.begin(); }
  iterator end() { return operands.end(); }
  const_iterator end() const { return operands.end(); }

  iterator insert(iterator I, const MCOperand &op) {
    return operands.insert(I, op);
  }

private:
  uint32_t opType = -1;
  std::vector<MCOperand> operands;
};

} // namespace mc

#endif // __MC_MCINST_H__
