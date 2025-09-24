#ifndef __MC_MCINST_H__
#define __MC_MCINST_H__

#include <cassert>
#include <cstdint>
#include <string>
#include <string_view>
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

class MCMemory {
public:
  constexpr MCMemory(uint64_t base, uint32_t off)
      : baseReg(base), offset(off) {}
  constexpr const MCRegister &getBaseReg() const { return baseReg; }
  constexpr uint32_t getOffset() const { return offset; }

private:
  MCRegister baseReg;
  uint32_t offset;
};

class MCLabel {
public:
  MCLabel(std::string_view lbl) : label(lbl) {}
  std::string_view getLabel() const { return label; }

private:
  std::string label;
};

class MCOperand {
  enum class MachineOperandType {
    kInvalid,
    kRegister,
    KImmediate, // Immediate integer value.
    kMemory,    // Memory operand.
    kLabel      // Label operand.
  };

  using OperandValue = std::variant<uint64_t,                      // RegVal
                                    int32_t,                       // ImmVal
                                    std::pair<uint64_t, uint32_t>, // MemVal
                                    std::string_view               // LabelVal
                                    >;

public:
  explicit MCOperand() : value(std::in_place_type<int32_t>, 0) {}
  bool isValid() const { return kind != MachineOperandType::kInvalid; }
  bool isReg() const { return kind == MachineOperandType::kRegister; }
  bool isImm() const { return kind == MachineOperandType::KImmediate; }
  bool isMem() const { return kind == MachineOperandType::kMemory; }
  bool isLabel() const { return kind == MachineOperandType::kLabel; }

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

  MCMemory getMem() const {
    assert(isMem() && "This is not a memory operand");
    auto [base, offset] = std::get<std::pair<uint64_t, uint32_t>>(value);
    return MCMemory(base, offset);
  }

  void setMem(const MCMemory &mem) {
    assert(isMem() && "This is not a memory operand");
    value = std::make_pair(mem.getBaseReg().id(), mem.getOffset());
  }

  MCLabel getLabel() const {
    assert(isLabel() && "This is not a label operand");
    return MCLabel(std::get<std::string_view>(value));
  }

  void setLabel(const MCLabel &lbl) {
    assert(isLabel() && "This is not a label operand");
    value = lbl.getLabel();
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

  static MCOperand createMem(uint64_t baseReg, uint32_t offset) {
    MCOperand op;
    op.kind = MachineOperandType::kMemory;
    op.value = std::make_pair(baseReg, offset);
    return op;
  }

  static MCOperand createLabel(std::string_view label) {
    MCOperand op;
    op.kind = MachineOperandType::kLabel;
    op.value = label;
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

  const std::vector<MCOperand> &getOperands() const { return operands; }
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
