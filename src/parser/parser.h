#pragma once
#include <vector>
#include <memory>
#include <string>
#include "lexer/lexer.h"
#include "ast/ast.h"

class Parser
{
public:
    Parser(const std::vector<Token> &tokens);
    StmtPtr parse();
    // Entry point: parse the whole program
    std::vector<StmtPtr> parseProgram();

private:
    const std::vector<Token> &tokens;
    size_t current = 0;

    // -------------------
    // Helpers
    // -------------------
    const Token &peek() const;
    const Token &previous() const;
    bool match(const std::vector<TokenType> &types);
    bool check(TokenType type) const;
    const Token &advance();
    bool isAtEnd() const;
    void consume(TokenType type, const std::string &msg);
    const Token &peekNext() const;

    // -------------------
    // Parsing rules
    // -------------------
    StmtPtr declaration();
    StmtPtr varDeclaration();
    StmtPtr statement();
    StmtPtr ifStatement();
    StmtPtr whileStatement();
    StmtPtr forStatement();
    StmtPtr timesStatement();
    StmtPtr returnStatement();
    StmtPtr expressionStatement();
    StmtPtr functionDeclaration();
    StmtPtr blockStatement();                // consumes '{'
    StmtPtr blockStatementAlreadyConsumed(); // assumes '{' already consumed
    StmtPtr finishBlock();

    ExprPtr expression();
    ExprPtr assignment();
    ExprPtr blockExpression();
    ExprPtr logicalOr();
    ExprPtr logicalAnd();
    ExprPtr equality();
    ExprPtr comparison();
    ExprPtr term();
    ExprPtr factor();
    ExprPtr unary();
    ExprPtr call();
    ExprPtr primary();
    ExprPtr arrayLiteral();

    ExprPtr bitwiseOr();
    ExprPtr bitwiseXor();
    ExprPtr bitwiseAnd();
    ExprPtr shift();

    std::string parseType();
    ExprPtr ifExpression();

    ExprPtr finishCall(ExprPtr callee);
    ExprPtr indexExpr(ExprPtr array);

    // -------------------
    // Utilities
    // -------------------
    void synchronize();
};
