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

void CompUnitAST::dump() const {
  std::cout << "CompUnitAST { ";
  for (size_t i = 0; i < topLevelNodes->size(); ++i) {
    if (i != 0) {
      std::cout << ", ";
    }
    (*topLevelNodes)[i]->dump();
  }
  std::cout << " }" << std::endl;
}

void FuncDefAST::dump() const {
  std::cout << "FuncDefAST { ";
  if (retType) {
    std::cout << toString(*retType.get());
  }
  std::cout << ", " << ident << ", ";
  if (block) {
    block->dump();
  }
  std::cout << " }";
}

/// Decleration
void DeclAST::dump() const {
  std::cout << "DeclAST { ";
  if (isConstDecl()) {
    constDecl->dump();
  } else if (isVarDecl()) {
    varDecl->dump();
  } else {
    assert(false && "Invalid DeclAST");
  }
  std::cout << " }";
}

void ConstDeclAST::dump() const {
  std::cout << "ConstDeclAST { ";
  std::cout << toString(*bType.get());
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

void VarDeclAST::dump() const {
  std::cout << "VarDeclAST { ";
  std::cout << toString(*bType.get());
  std::cout << ", [ ";
  for (size_t i = 0; i < varDefs->size(); ++i) {
    if (i != 0) {
      std::cout << ", ";
    }
    (*varDefs)[i]->dump();
  }
  std::cout << " ] }";
}

void VarDefAST::dump() const {
  std::cout << "VarDefAST { " << var << ", ";
  if (initVal) {
    initVal->dump();
  }
  std::cout << " }";
}

void InitValAST::dump() const {
  std::cout << "InitValAST { ";
  if (exp) {
    exp->dump();
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

/// Statement dumping
void ReturnStmtAST::dump() const {
  std::cout << "ReturnStmtAST { ";
  if (exp) {
    exp->dump();
  }
  std::cout << " }";
}

void AssignStmtAST::dump() const {
  std::cout << "AssignStmtAST { ";
  if (lVal) {
    lVal->dump();
  }
  std::cout << ", ";
  if (exp) {
    exp->dump();
  }
  std::cout << " }";
}

void ExprStmtAST::dump() const {
  std::cout << "ExprStmtAST { ";
  if (exp) {
    exp->dump();
  }
  std::cout << " }";
}

void BlockStmtAST::dump() const {
  std::cout << "BlockStmtAST { ";
  if (block) {
    block->dump();
  }
  std::cout << " }";
}

void IfStmtAST::dump() const {
  std::cout << "IfStmtAST { ";
  if (cond) {
    cond->dump();
  }
  if (thenStmt) {
    std::cout << ", ";
    thenStmt->dump();
  }
  if (elseStmt) {
    std::cout << ", ";
    elseStmt->dump();
  }
  std::cout << " }";
}

void WhileStmtAST::dump() const {
  std::cout << "WhileStmtAST { ";
  if (cond) {
    cond->dump();
  }
  if (body) {
    std::cout << ", ";
    body->dump();
  }
  std::cout << " }";
}

void BreakStmtAST::dump() const { std::cout << "BreakStmtAST { }"; }

void ContinueStmtAST::dump() const { std::cout << "ContinueStmtAST { }"; }

/// Expression dumping
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
  } else if (isFuncCall()) {
    funcCall->dump();
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

void FuncCallAST::dump() const {
  std::cout << "FuncCallAST { " << ident << ", [ ";
  if (hasParams()) {
    for (size_t i = 0; i < funcRParams->size(); ++i) {
      if (i != 0) {
        std::cout << ", ";
      }
      (*funcRParams)[i]->dump();
    }
  }
  std::cout << " ] }";
}

void FuncFParamAST::dump() const {
  std::cout << "FuncFParamAST { ";
  std::cout << toString(*type.get());
  std::cout << ", " << ident << " }";
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
