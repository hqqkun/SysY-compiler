#ifndef __UTILS_UTILS_H__
#define __UTILS_UTILS_H__

#include <cassert>
#include <list>
#include <string>
#include <unordered_map>
#include <variant>

#include "IR/Declaration.h"
#include "IR/Operation.h"
#include "IR/Value.h"

class SymbolTable {
public:
  explicit SymbolTable() = default;
  /// A value in the symbol table can be:
  /// - ir::Value* : a pointer to a variable.
  /// - int32_t : a constant integer value.
  /// - ir::Declaration* : a function declaration.
  using Val = std::variant<ir::Value *, int32_t, ir::Declaration *>;
  using Table = std::unordered_map<std::string, Val>;

  int32_t getConstant(const std::string &name) const {
    for (auto it = symbolTables.begin(); it != symbolTables.end(); ++it) {
      auto found = it->find(name);
      if (found != it->end() &&
          std::holds_alternative<int32_t>(found->second)) {
        return std::get<int32_t>(found->second);
      }
    }
    assert(false && "Constant not found in symbol table");
  }

  ir::Value *getValue(const std::string &name) const {
    for (auto it = symbolTables.begin(); it != symbolTables.end(); ++it) {
      auto found = it->find(name);
      if (found != it->end() &&
          std::holds_alternative<ir::Value *>(found->second)) {
        return std::get<ir::Value *>(found->second);
      }
    }
    assert(false && "Value not found in symbol table");
  }

  ir::Declaration *getDeclaration(const std::string &name) const {
    for (auto it = symbolTables.begin(); it != symbolTables.end(); ++it) {
      auto found = it->find(name);
      if (found != it->end() &&
          std::holds_alternative<ir::Declaration *>(found->second)) {
        return std::get<ir::Declaration *>(found->second);
      }
    }
    assert(false && "Function type not found in symbol table");
  }

  Val get(const std::string &name) const {
    for (auto it = symbolTables.begin(); it != symbolTables.end(); ++it) {
      auto found = it->find(name);
      if (found != it->end()) {
        return found->second;
      }
    }
    assert(false && "Symbol not found");
  }

  void setConstant(const std::string &name, int32_t value) {
    assert(!symbolTables.empty() && "No symbol table available");
    symbolTables.front()[name] = value;
  }

  void setValue(const std::string &name, ir::Value *value) {
    assert(!symbolTables.empty() && "No symbol table available");
    symbolTables.front()[name] = value;
  }

  void setDeclaration(const std::string &name, ir::Declaration *decl) {
    assert(!symbolTables.empty() && "No symbol table available");
    symbolTables.front()[name] = decl;
  }

  void enterScope() { symbolTables.emplace_front(); }
  void exitScope() {
    assert(!symbolTables.empty() && "No symbol table to exit");
    symbolTables.pop_front();
  }

  size_t depth() const { return symbolTables.size(); }

private:
  /// A list of symbol tables to represent scopes.
  /// The front of the list is the innermost scope.
  std::list<Table> symbolTables;
};

#endif // __UTILS_UTILS_H__
