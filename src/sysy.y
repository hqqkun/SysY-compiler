%code requires {
  #include <memory>
  #include <string>
  #include "AST/AST.h"
  #include "AST/Ops.h"
  #include "AST/Type.h"
}

%{
#include <iostream>
#include <memory>
#include <string>

#include "AST/AST.h"
#include "AST/Ops.h"
#include "AST/Type.h"

int yylex();
void yyerror(std::unique_ptr<ast::BaseAST>& ast, const char* s);
%}


%parse-param { std::unique_ptr<ast::BaseAST> &ast }

%union {
  int int_val;
  std::string* str_val;
  ast::BaseAST* ast_val;
  ast::Op op;
}


%token INT RETURN T_MUL T_DIV T_MOD T_ADD T_SUB T_BANG
%token <int_val> INT_CONST
%token <str_val> IDENT

%type <op> UnaryOp MulOp AddOp
%type <ast_val> FuncDef FuncType Block Stmt EXP PrimaryExp UnaryExp MulExp AddExp
%type <int_val> Number

%%

// Compilation unit
CompUnit
    : FuncDef {
      auto comp_unit = std::make_unique<ast::CompUnitAST>();
      comp_unit->funcDef = std::unique_ptr<ast::BaseAST>($1);
      ast = std::move(comp_unit);
    }
    ;

// FuncDef ::= FuncType IDENT '(' ')' Block;
FuncDef
    : FuncType IDENT '(' ')' Block {
      auto ast = new ast::FuncDefAST();
      ast->funcType = std::unique_ptr<ast::BaseAST>($1);
      ast->ident = *std::unique_ptr<std::string>($2);
      ast->block = std::unique_ptr<ast::BaseAST>($5);
      $$ = ast;
    }
    ;

FuncType
    : INT {
      auto ast = new ast::FuncTypeAST();
      ast->type = ast::Type::INT;
      $$ = ast;
    }
    ;

Block
    : '{' Stmt '}' {
      auto ast = new ast::BlockAST();
      ast->stmt = std::unique_ptr<ast::BaseAST>($2);
      $$ = ast;
    }
    ;

Stmt
    : RETURN EXP ';' {
      auto ast = new ast::StmtAST();
      ast->exp = std::unique_ptr<ast::BaseAST>($2);
      $$ = ast;
    }
    ;

EXP
    : AddExp {
      auto ast = new ast::ExprAST();
      ast->addExp = std::unique_ptr<ast::BaseAST>($1);
      $$ = ast;
    }
    ;

PrimaryExp
    : '(' EXP ')' {
      auto exp = std::unique_ptr<ast::BaseAST>($2);
      $$ = new ast::PrimaryExpAST(std::move(exp));
    }
    | Number {
      $$ = new ast::PrimaryExpAST($1);
    }
    ;

UnaryExp
    : PrimaryExp {
      auto exp = std::unique_ptr<ast::BaseAST>($1);
      $$ = new ast::UnaryExpAST(std::move(exp));
    }
    | UnaryOp UnaryExp {
      auto exp = std::unique_ptr<ast::BaseAST>($2);
      $$ = new ast::UnaryExpAST($1, std::move(exp));
    }
    ;

AddExp
    : MulExp {
      auto exp = std::unique_ptr<ast::BaseAST>($1);
      $$ = new ast::AddExpAST(std::move(exp));
    }
    | AddExp AddOp MulExp {
      auto left = std::unique_ptr<ast::BaseAST>($1);
      auto right = std::unique_ptr<ast::BaseAST>($3);
      $$ = new ast::AddExpAST(std::move(left), $2, std::move(right));
    }
    ;

MulExp
    : UnaryExp {
      auto exp = std::unique_ptr<ast::BaseAST>($1);
      $$ = new ast::MulExpAST(std::move(exp));
    }
    | MulExp MulOp UnaryExp {
      auto left = std::unique_ptr<ast::BaseAST>($1);
      auto right = std::unique_ptr<ast::BaseAST>($3);
      $$ = new ast::MulExpAST(std::move(left), $2, std::move(right));
    }
    ;



// Operators
UnaryOp
    : T_ADD { $$ = ast::Op::PLUS; }
    | T_SUB { $$ = ast::Op::MINUS; }
    | T_BANG { $$ = ast::Op::BANG; }
    ;

MulOp
    : T_MUL { $$ = ast::Op::MUL; }
    | T_DIV { $$ = ast::Op::DIV; }
    | T_MOD { $$ = ast::Op::MOD; }
    ;

AddOp
    : T_ADD { $$ = ast::Op::PLUS; }
    | T_SUB { $$ = ast::Op::MINUS; }
    ;

Number
    : INT_CONST {
      $$ = $1;
    }
    ;
%%

void yyerror(std::unique_ptr<ast::BaseAST>& ast, const char* s) {
  std::cerr << "Error: " << s << std::endl;
}
