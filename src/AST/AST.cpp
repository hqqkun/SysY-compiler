#include <cassert>
#include <iostream>

#include "AST/AST.h"
#include "AST/Type.h"

namespace ast {

DeclAST::DeclAST(ASTPtr decl) {
  if (dynamic_cast<ConstDeclAST *>(decl.get())) {
    constDecl = std::move(decl);
    type = Type::CONST;
  } else if (dynamic_cast<VarDeclAST *>(decl.get())) {
    varDecl = std::move(decl);
    type = Type::VAR;
  } else {
    assert(false && "decl is neither constDecl or varDecl.");
  }
}

BlockItemAST::BlockItemAST(ASTPtr item) {
  if (dynamic_cast<DeclAST *>(item.get())) {
    decl = std::move(item);
    type = Type::DECL;
  } else if (dynamic_cast<StmtAST *>(item.get())) {
    stmt = std::move(item);
    type = Type::STMT;
  } else {
    assert(false && "item is neither decl or stmt.");
  }
}

const Type *FuncDefAST::getReturnType() const {
  assert(retType && "Return type is null");
  return retType.get();
}

const Type *FuncDefAST::getParamType(size_t index) const {
  static const Type VOID_TYPE = Type(BaseType::VOID);
  if (!funcFParams || index >= funcFParams->size()) {
    assert(false && "Parameter index out of range");
    return &VOID_TYPE; // Unreachable
  }
  return (*funcFParams)[index]->type.get();
}

const std::vector<const Type *> FuncDefAST::getParamTypes() const {
  std::vector<const Type *> types;
  if (funcFParams) {
    for (const auto &param : *funcFParams) {
      types.push_back(param->type.get());
    }
  }
  return types;
}

bool isLogicalOp(Op op) { return op == Op::LAND || op == Op::LOR; }

std::vector<std::string> FuncDefAST::getParamNames() const {
  std::vector<std::string> names;
  if (funcFParams) {
    for (const auto &param : *funcFParams) {
      names.push_back(param->ident);
    }
  }
  return names;
}

} // namespace ast
