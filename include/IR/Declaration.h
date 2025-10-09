#ifndef __IR_DECLARATION_H__
#define __IR_DECLARATION_H__

#include <string>

#include "IR/IRContext.h"
#include "IR/Object.h"
#include "IR/Type.h"

namespace ir {
class Declaration : public IRObject {
public:
  explicit Declaration() = delete;
  virtual ~Declaration() = default;

  const std::string &getIdent() const { return ident; }
  bool isFunction() const { return kind == DeclKind::kFunction; }

protected:
  enum class DeclKind { kInvalid, kFunction };
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
    return context.create<FunctionDecl>(name, funcType);
  }

private:
  FunctionType *funcType;
};

} // namespace ir

#endif // __IR_DECLARATION_H__
