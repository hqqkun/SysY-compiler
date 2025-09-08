%code requires {
  #include <memory>
  #include <string>
}

%{
#include <iostream>
#include <memory>
#include <string>

int yylex();
void yyerror(std::unique_ptr<std::string>& ast, const char* s);
%}


%parse-param { std::unique_ptr<std::string>& ast }

%union {
  int int_val;
  std::string* str_val;
}


%token INT RETURN
%token <int_val> INT_CONST
%token <str_val> IDENT

%type <str_val> FuncDef FuncType Block Stmt Number

%%

// Compilation unit
CompUnit
    : FuncDef {
      ast = std::unique_ptr<std::string>($1);
    }
    ;

// FuncDef ::= FuncType IDENT '(' ')' Block;
FuncDef
    : FuncType IDENT '(' ')' Block {
      auto type = std::unique_ptr<std::string>($1);
      auto ident = std::unique_ptr<std::string>($2);
      auto block = std::unique_ptr<std::string>($5);
      $$ = new std::string(*type + " " + *ident + "()" + *block);
    }
    ;

FuncType
    : INT {
      $$ = new std::string("int");
    }
    ;

Block
    : '{' Stmt '}' {
      auto stmt = std::unique_ptr<std::string>($2);
      $$ = new std::string(" {\n" + *stmt + "}\n");
    }
    ;

Stmt 
    : RETURN Number ';' {
      auto number = std::unique_ptr<std::string>($2);
      $$ = new std::string("return " + *number + ";\n");
    }
    ;

Number
    : INT_CONST {
      $$ = new std::string(std::to_string($1));
    }
    ;
%%

void yyerror(std::unique_ptr<std::string>& ast, const char* s) {
    std::cerr << "Error: " << s << std::endl;
}
