#ifndef __AST_OPS_H__
#define __AST_OPS_H__

#include <cassert>
#include <ostream>
#include <string_view>
#include <unordered_map>

namespace ast {
enum class Op {
  BANG,
  DIV,
  EQ,
  GE,
  GT,
  LAND,
  LE,
  LOR,
  LT,
  NEQ,
  MINUS,
  MOD,
  MUL,
  OR,
  PLUS,
};

inline std::ostream &operator<<(std::ostream &os, const Op &op) {
  static std::unordered_map<Op, std::string_view> opToStr = {
      {Op::BANG, "BANG"}, {Op::DIV, "DIV"},     {Op::EQ, "EQ"},
      {Op::GE, "GE"},     {Op::GT, "GT"},       {Op::LAND, "LAND"},
      {Op::LE, "LE"},     {Op::LOR, "LOR"},     {Op::LT, "LT"},
      {Op::NEQ, "NEQ"},   {Op::MINUS, "MINUS"}, {Op::MOD, "MOD"},
      {Op::MUL, "MUL"},   {Op::OR, "OR"},       {Op::PLUS, "PLUS"},
  };
  auto it = opToStr.find(op);
  assert(it != opToStr.end() && "Unknown Op");
  os << it->second;
  return os;
}

} // namespace ast

#endif // __AST_OPS_H__
