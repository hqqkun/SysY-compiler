#ifndef __IR_DECLARATION_H__
#define __IR_DECLARATION_H__

#include <cassert>
#include <string>

#include "IR/IRContext.h"
#include "IR/Object.h"
#include "IR/Operation.h"
#include "IR/Type.h"

namespace ir {
class Declaration : public IRObject {
public:
  explicit Declaration() = delete;
  virtual ~Declaration() = default;

  const std::string &getIdent() const { return ident; }
  bool isFunction() const { return kind == DeclKind::kFunction; }
  bool isGlobalVar() const { return kind == DeclKind::kGlobalVar; }

protected:
  enum class DeclKind { kInvalid, kFunction, kGlobalVar };
  explicit Declaration(const std::string &ident = "",
                       DeclKind kind = DeclKind::kInvalid)
      : IRObject(), ident(ident), kind(kind) {}
  std::string ident;
  DeclKind kind;
};

class FunctionDecl : public Declaration {
public:
  explicit FunctionDecl(IRContext &context, const std::string &name = "",
                        FunctionType *funcType = nullptr)
      : Declaration(name, DeclKind::kFunction), funcType(funcType) {}
  FunctionType *getFunctionType() const { return funcType; }

  static FunctionDecl *create(IRContext &context, const std::string &name,
                              FunctionType *funcType) {
    assert(funcType && "FunctionDecl function type cannot be null");
    return context.create<FunctionDecl>(name, funcType);
  }

private:
  FunctionType *funcType;
};

class GlobalVarDecl : public Declaration {
public:
  explicit GlobalVarDecl(IRContext &context, const std::string &name = "",
                         GlobalAlloc *alloc = nullptr)
      : Declaration(name, DeclKind::kGlobalVar), allocation(alloc) {}
  GlobalAlloc *getAllocation() const { return allocation; }

  static GlobalVarDecl *create(IRContext &context, const std::string &name,
                               GlobalAlloc *alloc) {
    assert(alloc && "GlobalVarDecl allocation cannot be null");
    return context.create<GlobalVarDecl>(name, alloc);
  }

private:
  GlobalAlloc *allocation;
};

} // namespace ir

#endif // __IR_DECLARATION_H__
