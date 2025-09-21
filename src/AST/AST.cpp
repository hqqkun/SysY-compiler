#include <cassert>
#include <iostream>

#include "AST/AST.h"
#include "AST/Type.h"

namespace ast {

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

/// Decleration
void DeclAST::dump() const {
  std::cout << "DeclAST { ";
  if (constDecl) {
    constDecl->dump();
  }
  std::cout << " }";
}

void ConstDeclAST::dump() const {
  std::cout << "ConstDeclAST { ";
  switch (bType) {
    case Type::INT:
      std::cout << "INT";
      break;
  }
  std::cout << ", [ ";
  for (size_t i = 0; i < constDefs->size(); ++i) {
    if (i != 0) {
      std::cout << ", ";
    }
    (*constDefs)[i]->dump();
  }
  std::cout << " ] }";
}

void ConstDefAST::dump() const {
  std::cout << "ConstDefAST { " << var << ", ";
  if (initVal) {
    initVal->dump();
  }
  std::cout << " }";
}

void ConstInitValAST::dump() const {
  std::cout << "ConstInitValAST { ";
  if (constExp) {
    constExp->dump();
  }
  std::cout << " }";
}

void BlockAST::dump() const {
  std::cout << "BlockAST { ";
  for (const auto &item : *blockItems) {
    item->dump();
    std::cout << ", ";
  }
  std::cout << " }";
}

void BlockItemAST::dump() const {
  std::cout << "BlockItemAST { ";
  if (isDecl()) {
    decl->dump();
  } else if (isStmt()) {
    stmt->dump();
  } else {
    assert(false && "Invalid BlockItemAST");
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

void LValAST::dump() const { std::cout << "LValAST { " << ident << " }"; }

void PrimaryExpAST::dump() const {
  std::cout << "PrimaryExpAST { ";
  if (isLVal()) {
    lVal->dump();
  } else if (isExp()) {
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

void ConstExpAST::dump() const {
  std::cout << "ConstExpAST { ";
  if (exp) {
    exp->dump();
  }
  std::cout << " }";
}

} // namespace ast
