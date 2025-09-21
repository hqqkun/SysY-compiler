#include <cassert>
#include <iostream>

#include "AST/AST.h"
#include "AST/Type.h"

namespace ast {

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

void StmtAST::dump() const {
  std::cout << "StmtAST { ";
  if (exp) {
    exp->dump();
  }
  std::cout << " }";
}

void ExprAST::dump() const {
  std::cout << "ExprAST { ";
  if (exp) {
    exp->dump();
  }
  std::cout << " }";
}

void PrimaryExpAST::dump() const {
  std::cout << "PrimaryExpAST { ";
  if (isExp()) {
    exp->dump();
  } else if (isNumber()) {
    std::cout << number;
  } else {
    assert(false && "Invalid PrimaryExpAST");
  }
  std::cout << " }";
}

void UnaryExpAST::dump() const {
  std::cout << "UnaryExpAST { ";
  if (isPrimary()) {
    primaryExp->dump();
  } else if (isUnaryOp()) {
    std::cout << unaryOp << ": ";
    childUnaryExp->dump();
  } else {
    assert(false && "Invalid UnaryExpAST");
  }
  std::cout << " }";
}

void BinaryExpAST::dump() const {
  std::cout << getASTNameImpl() << " { ";
  if (isSingle()) {
    singleExp->dump();
  } else {
    std::cout << binOp << ": ";
    std::cout << "( ";
    compositeExp.first->dump();
    std::cout << ", ";
    compositeExp.second->dump();
    std::cout << " )";
  }
  std::cout << " }";
}

} // namespace ast
