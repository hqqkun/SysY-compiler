#ifndef __AST_TYPE_H__
#define __AST_TYPE_H__

#include <string>

namespace ast {
enum class Type {
  INT,
  VOID,
};

std::string toString(Type type);

} // namespace ast

#endif // __AST_TYPE_H__
