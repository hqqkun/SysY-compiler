#ifndef __CONVERSION_CONVERSION_H__
#define __CONVERSION_CONVERSION_H__

#include "AST/AST.h"
#include "IR/Function.h"
#include "IR/IRContext.h"

namespace conversion {

ir::Function *convertASTToIR(ir::IRContext &context,
                             std::unique_ptr<ast::BaseAST> &ast);

} // namespace conversion

#endif // __CONVERSION_CONVERSION_H__
