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


%token INT RETURN T_MUL T_DIV T_MOD T_ADD T_SUB T_BANG T_LE T_GE T_LT T_GT T_EQ T_NEQ T_AND T_OR
%token <int_val> INT_CONST
%token <str_val> IDENT

%type <op> UnaryOp MulOp AddOp RelOp EqOp LAndOp LOrOp
%type <ast_val> FuncDef FuncType Block Stmt EXP PrimaryExp UnaryExp MulExp AddExp RelExp EqExp LAndExp LOrExp
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
    : LOrExp {
      auto ast = new ast::ExprAST();
      ast->exp = std::unique_ptr<ast::BaseAST>($1);
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

RelExp
    : AddExp {
      auto exp = std::unique_ptr<ast::BaseAST>($1);
      $$ = new ast::RelExpAST(std::move(exp));
    }
    | RelExp RelOp AddExp {
      auto left = std::unique_ptr<ast::BaseAST>($1);
      auto right = std::unique_ptr<ast::BaseAST>($3);
      $$ = new ast::RelExpAST(std::move(left), $2, std::move(right));
    }
    ;

EqExp
    : RelExp {
      auto exp = std::unique_ptr<ast::BaseAST>($1);
      $$ = new ast::EqExpAST(std::move(exp));
    }
    | EqExp EqOp RelExp {
      auto left = std::unique_ptr<ast::BaseAST>($1);
      auto right = std::unique_ptr<ast::BaseAST>($3);
      $$ = new ast::EqExpAST(std::move(left), $2, std::move(right));
    }
    ;

LAndExp
    : EqExp {
      auto exp = std::unique_ptr<ast::BaseAST>($1);
      $$ = new ast::LAndExpAST(std::move(exp));
    }
    | LAndExp LAndOp EqExp {
      auto left = std::unique_ptr<ast::BaseAST>($1);
      auto right = std::unique_ptr<ast::BaseAST>($3);
      $$ = new ast::LAndExpAST(std::move(left), $2, std::move(right));
    }
    ;

LOrExp
    : LAndExp {
      auto exp = std::unique_ptr<ast::BaseAST>($1);
      $$ = new ast::LOrExpAST(std::move(exp));
    }
    | LOrExp LOrOp LAndExp {
      auto left = std::unique_ptr<ast::BaseAST>($1);
      auto right = std::unique_ptr<ast::BaseAST>($3);
      $$ = new ast::LOrExpAST(std::move(left), $2, std::move(right));
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

RelOp
    : T_LT { $$ = ast::Op::LT; }
    | T_GT { $$ = ast::Op::GT; }
    | T_LE { $$ = ast::Op::LE; }
    | T_GE { $$ = ast::Op::GE; }
    ;

EqOp
    : T_EQ { $$ = ast::Op::EQ; }
    | T_NEQ { $$ = ast::Op::NEQ; }
    ;

LAndOp
    : T_AND { $$ = ast::Op::LAND; }
    ;

LOrOp
    : T_OR { $$ = ast::Op::LOR; }
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
