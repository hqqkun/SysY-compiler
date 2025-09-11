#include <cstdio>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>

#include "AST/AST.h"
#include "Conversion/Conversion.h"

extern FILE *yyin;
extern int yyparse(std::unique_ptr<ast::BaseAST> &ast);

int main(int argc, const char *argv[]) {
  if (argc != 5) {
    std::cerr << "Usage: " << argv[0] << " -koopa <input file> -o <output file>"
              << std::endl;
    return -1;
  }
  [[maybe_unused]] const char *mode = argv[1];
  const char *input_file = argv[2];
  const char *output_file = argv[4];

  yyin = std::fopen(input_file, "r");
  if (!yyin) {
    std::cerr << "Failed to open input file: " << input_file << std::endl;
    return -1;
  }
  std::ofstream ofs(output_file);
  if (!ofs.is_open()) {
    std::cerr << "Failed to open output file: " << output_file << std::endl;
    return -1;
  }

  std::unique_ptr<ast::BaseAST> ast;
  int parse_result = yyparse(ast);
  if (parse_result != 0) {
    std::cerr << "Parsing failed with error code: " << parse_result
              << std::endl;
    return -1;
  }

  ir::IRContext irContext;
  ir::Function *func = conversion::convertASTToIR(irContext, ast);
  if (!func) {
    std::cerr << "Failed to convert AST to IR." << std::endl;
    return -1;
  }

  func->print(ofs);
  return 0;
}
