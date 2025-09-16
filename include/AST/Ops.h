#ifndef __AST_OPS_H__
#define __AST_OPS_H__

#include <ostream>

namespace ast {
enum class Op {
  PLUS,
  MINUS,
  BANG,
  DIV,
  MUL,
  MOD,
};

inline std::ostream &operator<<(std::ostream &os, const Op &op) {
  switch (op) {
    case Op::PLUS:
      os << "PLUS";
      break;
    case Op::MINUS:
      os << "MINUS";
      break;
    case Op::BANG:
      os << "BANG";
      break;
    case Op::DIV:
      os << "DIV";
      break;
    case Op::MUL:
      os << "MUL";
      break;
    case Op::MOD:
      os << "MOD";
      break;
  }
  return os;
}

} // namespace ast

#endif // __AST_OPS_H__
