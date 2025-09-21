#ifndef __UTILS_UTILS_H__
#define __UTILS_UTILS_H__

#include <cassert>
#include <string>
#include <unordered_map>
#include <vector>

class SymbolTable {
public:
  explicit SymbolTable() { symbolTables.emplace_back(); }

  int32_t getConstant(const std::string &name) const {
    for (auto it = symbolTables.rbegin(); it != symbolTables.rend(); ++it) {
      auto found = it->find(name);
      if (found != it->end()) {
        return found->second;
      }
    }
    assert(false && "Constant not found");
  }

  void setConstant(const std::string &name, int32_t value) {
    assert(!symbolTables.empty() && "No symbol table available");
    symbolTables.back()[name] = value;
  }

private:
  using Table = std::unordered_map<std::string, int32_t>;
  std::vector<Table> symbolTables;
};

#endif // __UTILS_UTILS_H__
