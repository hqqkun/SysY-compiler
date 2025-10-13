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
  ast::ConstInitValAST* constInitVal;
  ast::ConstExpAST* constExp;
  ast::InitValAST* initVal;
  ast::BlockAST* block_val;
  std::vector<ast::ASTPtr>* astVec;
  std::vector<ast::BlockItemPtr>* blockItemVec;
  std::vector<ast::FuncFParamPtr>* funcFParamVec;
  std::vector<ast::FuncRParamPtr>* funcRParamVec;
  std::vector<ast::ConstExpPtr>* constExpVec;
  std::vector<ast::ExprPtr>* exprVec;
  ast::FuncFParamAST* funcFParam;
  ast::BlockItemAST* blockItem;
  ast::Op op;
  ast::Type* type;
}


%token INT IF ELSE RETURN CONST WHILE BREAK CONTINUE VOID T_MUL T_DIV T_MOD T_ADD T_SUB T_BANG T_LE T_GE T_LT T_GT T_EQ T_NEQ T_AND T_OR T_ASSIGN
%token <int_val> INT_CONST
%token <str_val> IDENT

%type <op> UnaryOp MulOp AddOp RelOp EqOp LAndOp LOrOp

%type <ast_val> FuncDef Block Stmt Decl ConstDecl VarDecl ConstDef VarDef
%type <constInitVal> ConstInitVal
%type <initVal> InitVal
%type <ast_val> EXP PrimaryExp UnaryExp MulExp AddExp RelExp EqExp LAndExp LOrExp LVal
%type <constExp> ConstExp
%type <astVec> ConstDefList
%type <astVec> VarDefList
%type <astVec> CompUnitList
%type <constExpVec> ConstExpList
%type <exprVec> ExprList
%type <blockItemVec> BlockItemList
%type <funcFParamVec> FuncFParamList
%type <funcFParam> FuncFParam
%type <funcRParamVec> FuncRParamList
%type <blockItem> BlockItem
%type <type> Type
%type <int_val> Number

%nonassoc THEN
%nonassoc ELSE

%%

// Compilation unit
CompUnit
    : CompUnitList {
      auto vec = std::unique_ptr<std::vector<ast::ASTPtr>>($1);
      auto comp_unit = std::make_unique<ast::CompUnitAST>(std::move(vec));
      ast = std::move(comp_unit);
    }
    ;

CompUnitList
    : FuncDef {
      auto vec = new std::vector<ast::ASTPtr>();
      vec->emplace_back($1);
      $$ = vec;
    }
    | Decl {
      auto vec = new std::vector<ast::ASTPtr>();
      vec->emplace_back($1);
      $$ = vec;
    }
    | CompUnitList FuncDef {
      auto vec = $1;
      vec->emplace_back($2);
      $$ = vec;
    }
    | CompUnitList Decl {
      auto vec = $1;
      vec->emplace_back($2);
      $$ = vec;
    }
    ;

// FuncDef ::= Type IDENT '(' [FuncFParamList] ')' Block;
FuncDef
    : Type IDENT '(' ')' Block {
      auto funcType = std::unique_ptr<ast::Type>($1);
      auto ident = *std::unique_ptr<std::string>($2);
      auto block = std::unique_ptr<ast::BaseAST>($5);
      $$ = new ast::FuncDefAST(std::move(funcType), ident, std::move(block));
    }
    | Type IDENT '(' FuncFParamList ')' Block {
      auto funcType = std::unique_ptr<ast::Type>($1);
      auto ident = *std::unique_ptr<std::string>($2);
      auto params = std::unique_ptr<std::vector<ast::FuncFParamPtr>>($4);
      auto block = std::unique_ptr<ast::BaseAST>($6);
      $$ = new ast::FuncDefAST(std::move(funcType), ident, std::move(params), std::move(block));
    }
    ;

FuncFParamList
    : FuncFParam {
      auto vec = new std::vector<ast::FuncFParamPtr>();
      vec->emplace_back($1);
      $$ = vec;
    }
    | FuncFParamList ',' FuncFParam {
      auto vec = $1;
      vec->emplace_back($3);
      $$ = vec;
    }
    ;

FuncFParam
    : Type IDENT {
      auto type = std::unique_ptr<ast::Type>($1);
      auto ident = *std::unique_ptr<std::string>($2);
      $$ = new ast::FuncFParamAST(std::move(type), ident);
    }

Type
    : INT {
      $$ = new ast::Type(ast::BaseType::INT);
    }
    | VOID {
      $$ = new ast::Type(ast::BaseType::VOID);
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
    : CONST Type ConstDefList ';' {
      auto type = std::unique_ptr<ast::Type>($2);
      auto defList = std::unique_ptr<std::vector<ast::ASTPtr>>($3);
      $$ = new ast::ConstDeclAST(std::move(type), std::move(defList));
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
    : Type VarDefList ';' {
      auto type = std::unique_ptr<ast::Type>($1);
      auto defList = std::unique_ptr<std::vector<ast::ASTPtr>>($2);
      $$ = new ast::VarDeclAST(std::move(type), std::move(defList));
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
      // Variable with initialization.
      auto var = *std::unique_ptr<std::string>($1);
      auto init = std::unique_ptr<ast::ConstInitValAST>($3);
      $$ = new ast::ConstDefAST(var, std::move(init));
    }
    | IDENT '[' ConstExp ']' T_ASSIGN ConstInitVal {
      // Array variable with initialization.
      auto var = *std::unique_ptr<std::string>($1);
      auto size = std::unique_ptr<ast::ConstExpAST>($3);
      auto init = std::unique_ptr<ast::ConstInitValAST>($6);
      $$ = new ast::ConstDefAST(var, std::move(size), std::move(init));
    }
    ;

ConstInitVal
    : ConstExp {
      auto exp = std::unique_ptr<ast::ConstExpAST>($1);
      $$ = new ast::ConstInitValAST(std::move(exp));
    }
    | '{' ConstExpList '}' {
      auto list = std::unique_ptr<std::vector<ast::ConstExpPtr>>($2);
      $$ = new ast::ConstInitValAST(std::move(list));
    }
    ;

ConstExpList
    : /* empty */ {
      auto vec = new std::vector<ast::ConstExpPtr>();
      $$ = vec;
    }
    | ConstExp {
      auto vec = new std::vector<ast::ConstExpPtr>();
      vec->emplace_back($1);
      $$ = vec;
    }
    | ConstExpList ',' ConstExp {
      auto vec = $1;
      vec->emplace_back($3);
      $$ = vec;
    }

VarDef
    : IDENT {
      auto var = *std::unique_ptr<std::string>($1);
      $$ = new ast::VarDefAST(var);
    }
    | IDENT T_ASSIGN InitVal {
      auto var = *std::unique_ptr<std::string>($1);
      auto init = std::unique_ptr<ast::InitValAST>($3);
      $$ = new ast::VarDefAST(var, std::move(init));
    }
    | IDENT '[' ConstExp ']' {
      auto var = *std::unique_ptr<std::string>($1);
      auto size = std::unique_ptr<ast::ConstExpAST>($3);
      $$ = new ast::VarDefAST(var, std::move(size));
    }
    | IDENT '[' ConstExp ']' T_ASSIGN InitVal {
      auto var = *std::unique_ptr<std::string>($1);
      auto size = std::unique_ptr<ast::ConstExpAST>($3);
      auto init = std::unique_ptr<ast::InitValAST>($6);
      $$ = new ast::VarDefAST(var, std::move(size), std::move(init));
    }
    ;

InitVal
    : EXP {
      auto exp = std::unique_ptr<ast::ExprAST>(static_cast<ast::ExprAST*>($1));
      $$ = new ast::InitValAST(std::move(exp));
    }
    | '{' ExprList '}' {
      auto list = std::unique_ptr<std::vector<ast::ExprPtr>>($2);
      $$ = new ast::InitValAST(std::move(list));
    }
    ;

ExprList
    : /* empty */ {
      auto vec = new std::vector<ast::ExprPtr>();
      $$ = vec;
    }
    | EXP {
      auto vec = new std::vector<ast::ExprPtr>();
      vec->emplace_back(static_cast<ast::ExprAST*>($1));
      $$ = vec;
    }
    | ExprList ',' EXP {
      auto vec = $1;
      vec->emplace_back(static_cast<ast::ExprAST*>($3));
      $$ = vec;
    }

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
    | IDENT '[' EXP ']' {
      auto var = *std::unique_ptr<std::string>($1);
      auto index = std::unique_ptr<ast::ExprAST>(static_cast<ast::ExprAST*>($3));
      $$ = new ast::LValAST(var, std::move(index));
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
    | IDENT '(' ')' {
      auto ident = *std::unique_ptr<std::string>($1);
      auto funcCall = std::make_unique<ast::FuncCallAST>(ident);
      $$ = new ast::UnaryExpAST(std::move(funcCall));
    }
    | IDENT '(' FuncRParamList ')' {
      auto ident = *std::unique_ptr<std::string>($1);
      auto params = std::unique_ptr<std::vector<ast::FuncRParamPtr>>($3);
      auto funcCall = std::make_unique<ast::FuncCallAST>(ident, std::move(params));
      $$ = new ast::UnaryExpAST(std::move(funcCall));
    }
    ;

FuncRParamList
    : EXP {
      auto vec = new std::vector<ast::FuncRParamPtr>();
      vec->emplace_back(static_cast<ast::ExprAST*>($1));
      $$ = vec;
    }
    | FuncRParamList ',' EXP {
      auto vec = $1;
      vec->emplace_back(static_cast<ast::ExprAST*>($3));
      $$ = vec;
    }

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
