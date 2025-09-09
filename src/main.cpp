#include <cassert>
#include <cstdio>
#include <iostream>
#include <memory>
#include <string>

extern FILE *yyin;
extern int yyparse(std::unique_ptr<std::string> &ast);

int main(int argc, const char *argv[]) {
  assert(argc == 5);
  const char *mode = argv[1];
  const char *input_file = argv[2];
  const char *output_file = argv[4];

  yyin = fopen(input_file, "r");
  assert(yyin);

  std::unique_ptr<std::string> ast;
  int parse_result = yyparse(ast);
  assert(parse_result == 0);

  std::cout << *ast << std::endl;
  return 0;
}
