#pragma once
#include <vector>
#include <memory>
#include "../lexer/lexer.h"
#include "../ast/ast.h"

class Parser {
    const std::vector<Token> tokens;
    size_t current;

    Token peek() const;
    Token previous() const;
    Token advance();
    bool isAtEnd() const;
    bool check(TokenType t) const;
    bool match(TokenType t);
    void consume(TokenType t, const std::string &errMsg);

    // expression parsing (precedence)
    std::unique_ptr<Expr> expression();
    std::unique_ptr<Expr> orExpr();
    std::unique_ptr<Expr> andExpr();
    std::unique_ptr<Expr> equality();
    std::unique_ptr<Expr> comparison();
    std::unique_ptr<Expr> term();
    std::unique_ptr<Expr> factor();
    std::unique_ptr<Expr> unary();
    std::unique_ptr<Expr> call();
    std::unique_ptr<Expr> primary();

    // statements
    std::unique_ptr<Stmt> declaration();
    std::unique_ptr<Stmt> statement();
    std::unique_ptr<Stmt> letDeclaration();
    std::unique_ptr<Stmt> varDeclaration();
    std::unique_ptr<BlockStmt> block();
    std::unique_ptr<Stmt> ifStatement();
    std::unique_ptr<Stmt> exprStatement();

public:
    Parser(const std::vector<Token> &toks);
    std::vector<std::unique_ptr<Stmt>> parse();

    // debug helper: print AST
    static void printAST(const std::vector<std::unique_ptr<Stmt>> &program, int indent = 0);
};
