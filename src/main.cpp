#include <cstdio>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>

#include "AST/AST.h"
#include "Conversion/IRGen.h"
#include "Conversion/IRPrinter.h"
#include "Target/RISCV/RISCVTargetMachine.h"

static constexpr std::string_view kIR = "-koopa";
static constexpr std::string_view kRISCV = "-riscv";

extern FILE *yyin;
extern int yyparse(std::unique_ptr<ast::BaseAST> &ast);

int main(int argc, const char *argv[]) {
  if (argc != 5) {
    std::cerr << "Usage: " << argv[0] << " -koopa <input file> -o <output file>"
              << std::endl;
    return -1;
  }
  std::string mode = argv[1];
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
  conversion::IRGen irGen(irContext);
  ir::Function *func = irGen.generate(ast);
  if (!func) {
    std::cerr << "Failed to convert AST to IR." << std::endl;
    return -1;
  }

  if (mode == kIR) {
    conversion::IRPrinter printer(ofs);
    printer.printFunction(func);
  } else if (mode == kRISCV) {
    target::riscv::RISCVTargetMachine tm(ofs);
    tm.codeGen(func);
  } else {
    std::cerr << "Unknown mode: " << mode << std::endl;
    return -1;
  }
  return 0;
}
