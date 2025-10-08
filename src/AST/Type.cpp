#include "AST/Type.h"
#include <cassert>

namespace ast {

std::string toString(Type type) {
  switch (type) {
    case Type::INT:
      return "INT";
    case Type::VOID:
      return "VOID";
    default:
      assert(false && "Unknown Type");
  }
}
} // namespace ast
