#ifndef __UTILS_UTILS_H__
#define __UTILS_UTILS_H__

#include <cassert>
#include <list>
#include <string>
#include <unordered_map>
#include <variant>

#include "IR/Operation.h"
#include "IR/Value.h"

class SymbolTable {
public:
  explicit SymbolTable() { symbolTables.emplace_back(); }
  using Val = std::variant<ir::Value *, int32_t>;
  using Table = std::unordered_map<std::string, Val>;

  int32_t getConstant(const std::string &name) const {
    for (auto it = symbolTables.rbegin(); it != symbolTables.rend(); ++it) {
      auto found = it->find(name);
      if (found != it->end() &&
          std::holds_alternative<int32_t>(found->second)) {
        return std::get<int32_t>(found->second);
      }
    }
    assert(false && "Constant not found in symbol table");
  }

  ir::Value *getValue(const std::string &name) const {
    for (auto it = symbolTables.rbegin(); it != symbolTables.rend(); ++it) {
      auto found = it->find(name);
      if (found != it->end() &&
          std::holds_alternative<ir::Value *>(found->second)) {
        return std::get<ir::Value *>(found->second);
      }
    }
    assert(false && "Value not found in symbol table");
  }

  Val get(const std::string &name) const {
    for (auto it = symbolTables.rbegin(); it != symbolTables.rend(); ++it) {
      auto found = it->find(name);
      if (found != it->end()) {
        return found->second;
      }
    }
    assert(false && "Symbol not found");
  }

  void setConstant(const std::string &name, int32_t value) {
    assert(!symbolTables.empty() && "No symbol table available");
    symbolTables.back()[name] = value;
  }

  void setValue(const std::string &name, ir::Value *value) {
    assert(!symbolTables.empty() && "No symbol table available");
    symbolTables.back()[name] = value;
  }

private:
  std::list<Table> symbolTables;
};

#endif // __UTILS_UTILS_H__
