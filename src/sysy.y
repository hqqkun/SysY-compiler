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
  ast::BlockAST* block_val;
  std::vector<ast::ASTPtr>* astVec;
  std::vector<ast::BlockItemPtr>* blockItemVec;
  ast::BlockItemAST* blockItem;
  ast::Op op;
  ast::Type type;
}


%token INT IF ELSE RETURN CONST WHILE BREAK CONTINUE T_MUL T_DIV T_MOD T_ADD T_SUB T_BANG T_LE T_GE T_LT T_GT T_EQ T_NEQ T_AND T_OR T_ASSIGN
%token <int_val> INT_CONST
%token <str_val> IDENT

%type <op> UnaryOp MulOp AddOp RelOp EqOp LAndOp LOrOp

%type <ast_val> FuncDef FuncType Block Stmt Decl ConstDecl VarDecl ConstDef VarDef ConstInitVal InitVal
%type <ast_val> EXP PrimaryExp UnaryExp MulExp AddExp RelExp EqExp LAndExp LOrExp ConstExp LVal
%type <astVec> ConstDefList
%type <astVec> VarDefList
%type <blockItemVec> BlockItemList
%type <blockItem> BlockItem
%type <type> BType
%type <int_val> Number

%nonassoc THEN
%nonassoc ELSE

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

BType
    : INT {
      $$ = ast::Type::INT;
    }
    ;

/// Declaration
Decl
    : ConstDecl {
      auto decl = std::unique_ptr<ast::BaseAST>($1);
      $$ = new ast::DeclAST(std::move(decl));
    }
    | VarDecl {
      auto decl = std::unique_ptr<ast::BaseAST>($1);
      $$ = new ast::DeclAST(std::move(decl));
    }
    ;

ConstDecl
    : CONST BType ConstDefList ';' {
      auto defList = std::unique_ptr<std::vector<ast::ASTPtr>>($3);
      $$ = new ast::ConstDeclAST($2, std::move(defList));
    }
    ;

ConstDefList
    : ConstDef {
      auto vec = new std::vector<std::unique_ptr<ast::BaseAST>>();
      vec->emplace_back($1);
      $$ = vec;
    }
    | ConstDefList ',' ConstDef {
      auto vec = $1;
      vec->emplace_back($3);
      $$ = vec;
    }
    ;

VarDecl
    : BType VarDefList ';' {
      auto defList = std::unique_ptr<std::vector<ast::ASTPtr>>($2);
      $$ = new ast::VarDeclAST($1, std::move(defList));
    }
    ;

VarDefList
    : VarDef {
      auto vec = new std::vector<std::unique_ptr<ast::BaseAST>>();
      vec->emplace_back($1);
      $$ = vec;
    }
    | VarDefList ',' VarDef {
      auto vec = $1;
      vec->emplace_back($3);
      $$ = vec;
    }
    ;

ConstDef
    : IDENT T_ASSIGN ConstInitVal {
      auto var = *std::unique_ptr<std::string>($1);
      auto init = std::unique_ptr<ast::BaseAST>($3);
      $$ = new ast::ConstDefAST(var, std::move(init));
    }
    ;

ConstInitVal
    : ConstExp {
      auto exp = std::unique_ptr<ast::BaseAST>($1);
      $$ = new ast::ConstInitValAST(std::move(exp));
    }
    ;

VarDef
    : IDENT {
      auto var = *std::unique_ptr<std::string>($1);
      $$ = new ast::VarDefAST(var);
    }
    | IDENT T_ASSIGN InitVal {
      auto var = *std::unique_ptr<std::string>($1);
      auto init = std::unique_ptr<ast::BaseAST>($3);
      $$ = new ast::VarDefAST(var, std::move(init));
    }
    ;

InitVal
    : EXP {
      auto exp = std::unique_ptr<ast::BaseAST>($1);
      $$ = new ast::InitValAST(std::move(exp));
    }
    ;

Block
    : '{' BlockItemList '}' {
      auto list = std::unique_ptr<std::vector<ast::BlockItemPtr>>($2);
      $$ = new ast::BlockAST(std::move(list));
    }
    ;

BlockItemList
    : /* empty */ {
      auto vec = new std::vector<ast::BlockItemPtr>();
      $$ = vec;
    }
    | BlockItemList BlockItem {
      auto vec = $1;
      vec->emplace_back($2);
      $$ = vec;
    }
    ;

BlockItem
    : Decl {
      auto item = std::unique_ptr<ast::BaseAST>($1);
      $$ = new ast::BlockItemAST(std::move(item));
    }
    | Stmt {
      auto item = std::unique_ptr<ast::BaseAST>($1);
      $$ = new ast::BlockItemAST(std::move(item));
    }
    ;

Stmt
    : RETURN EXP ';' {
      auto exp = std::unique_ptr<ast::BaseAST>($2);
      $$ = new ast::ReturnStmtAST(std::move(exp));
    }
    | RETURN ';' {
      $$ = new ast::ReturnStmtAST();
    }
    | LVal T_ASSIGN EXP ';' {
      auto var = std::unique_ptr<ast::BaseAST>($1);
      auto exp = std::unique_ptr<ast::BaseAST>($3);
      $$ = new ast::AssignStmtAST(std::move(var), std::move(exp));
    }
    | Block {
      auto block = std::unique_ptr<ast::BlockAST>(static_cast<ast::BlockAST*>($1));
      $$ = new ast::BlockStmtAST(std::move(block));
    }
    | IF '(' EXP ')' Stmt %prec THEN {
      auto cond = std::unique_ptr<ast::ExprAST>(static_cast<ast::ExprAST*>($3));
      auto thenStmt = std::unique_ptr<ast::StmtAST>(static_cast<ast::StmtAST*>($5));
      $$ = new ast::IfStmtAST(std::move(cond), std::move(thenStmt));
    }
    | IF '(' EXP ')' Stmt ELSE Stmt {
      auto cond = std::unique_ptr<ast::ExprAST>(static_cast<ast::ExprAST*>($3));
      auto thenStmt = std::unique_ptr<ast::StmtAST>(static_cast<ast::StmtAST*>($5));
      auto elseStmt = std::unique_ptr<ast::StmtAST>(static_cast<ast::StmtAST*>($7));
      $$ = new ast::IfStmtAST(std::move(cond), std::move(thenStmt), std::move(elseStmt));
    }
    | WHILE '(' EXP ')' Stmt {
      auto cond = std::unique_ptr<ast::ExprAST>(static_cast<ast::ExprAST*>($3));
      auto body = std::unique_ptr<ast::StmtAST>(static_cast<ast::StmtAST*>($5));
      $$ = new ast::WhileStmtAST(std::move(cond), std::move(body));
    }
    | BREAK ';' {
      $$ = new ast::BreakStmtAST();
    }
    | CONTINUE ';' {
      $$ = new ast::ContinueStmtAST();
    }
    | EXP ';' {
      auto exp = std::unique_ptr<ast::BaseAST>($1);
      $$ = new ast::ExprStmtAST(std::move(exp));
    }
    | ';' {
      // Empty statement.
      $$ = new ast::ExprStmtAST();
    }
    ;

/// Expression
LVal
    : IDENT {
      auto var = *std::unique_ptr<std::string>($1);
      $$ = new ast::LValAST(var);
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
    | LVal {
      auto lval = std::unique_ptr<ast::BaseAST>($1);
      $$ = new ast::PrimaryExpAST(std::move(lval));
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

ConstExp
    : EXP {
      auto exp = std::unique_ptr<ast::BaseAST>($1);
      $$ = new ast::ConstExpAST(std::move(exp));
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
