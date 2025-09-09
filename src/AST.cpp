#include <iostream>

#include "ast.h"
#include "type.h"

void CompUnitAST::dump() const {
  std::cout << "CompUnitAST { ";
  if (funcDef) {
    funcDef->dump();
  }
  std::cout << " }" << std::endl;
}

void FuncDefAST::dump() const {
  std::cout << "FuncDefAST { ";
  if (funcType) {
    funcType->dump();
  }
  std::cout << ", " << ident << ", ";
  if (block) {
    block->dump();
  }
  std::cout << " }";
}

void FuncTypeAST::dump() const {
  std::cout << "FuncTypeAST { ";
  switch (type) {
  case Type::INT:
    std::cout << "INT";
    break;
  }
  std::cout << " }";
}

void BlockAST::dump() const {
  std::cout << "BlockAST { ";
  if (stmt) {
    stmt->dump();
  }
  std::cout << " }";
}

void StmtAST::dump() const { std::cout << "StmtAST { " << number << " }"; }
