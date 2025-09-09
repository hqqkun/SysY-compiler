%code requires {
  #include <memory>
  #include <string>
  #include "ast.h"
  #include "type.h"
}

%{
#include <iostream>
#include <memory>
#include <string>

#include "ast.h"
#include "type.h"

int yylex();
void yyerror(std::unique_ptr<BaseAST>& ast, const char* s);
%}


%parse-param { std::unique_ptr<BaseAST> &ast }

%union {
  int int_val;
  std::string* str_val;
  BaseAST* ast_val;
}


%token INT RETURN
%token <int_val> INT_CONST
%token <str_val> IDENT

%type <ast_val> FuncDef FuncType Block Stmt
%type <int_val> Number

%%

// Compilation unit
CompUnit
    : FuncDef {
      auto comp_unit = std::make_unique<CompUnitAST>();
      comp_unit->funcDef = std::unique_ptr<BaseAST>($1);
      ast = std::move(comp_unit);
    }
    ;

// FuncDef ::= FuncType IDENT '(' ')' Block;
FuncDef
    : FuncType IDENT '(' ')' Block {
      auto ast = new FuncDefAST();
      ast->funcType = std::unique_ptr<BaseAST>($1);
      ast->ident = *std::unique_ptr<std::string>($2);
      ast->block = std::unique_ptr<BaseAST>($5);
      $$ = ast;
    }
    ;

FuncType
    : INT {
      auto ast = new FuncTypeAST();
      ast->type = Type::INT;
      $$ = ast;
    }
    ;

Block
    : '{' Stmt '}' {
      auto ast = new BlockAST();
      ast->stmt = std::unique_ptr<BaseAST>($2);
      $$ = ast;
    }
    ;

Stmt
    : RETURN Number ';' {
      auto ast = new StmtAST();
      ast->number = $2;
      $$ = ast;
    }
    ;

Number
    : INT_CONST {
      $$ = $1;
    }
    ;
%%

void yyerror(std::unique_ptr<BaseAST>& ast, const char* s) {
  std::cerr << "Error: " << s << std::endl;
}
