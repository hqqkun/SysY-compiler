#include "AST/Type.h"
#include <cassert>

namespace ast {

std::string toString(const Type &type) {
  switch (type.kind) {
    case Type::Kind::BASE: {
      switch (type.base) {
        case BaseType::INT:
          return "INT";
        case BaseType::VOID:
          return "VOID";
        default:
          assert(false && "Unsupported AST base type");
          return "UNKNOWN";
      }
    }
    case Type::Kind::POINTER: {
      if (!type.pointee) {
        assert(false && "Pointee type cannot be null");
      }
      return "PTR to " + toString(*type.pointee);
    }
    default:
      assert(false && "Unsupported AST type kind");
      return "UNKNOWN";
  }
}
} // namespace ast
