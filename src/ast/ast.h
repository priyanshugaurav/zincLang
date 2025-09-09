#pragma once
#include <string>
#include <vector>
#include <memory>
#include "../lexer/lexer.h"

// === AST ===
// Base classes
struct Expr {
    virtual ~Expr() = default;
};

struct Stmt {
    virtual ~Stmt() = default;
};

// ----- Expressions -----
struct LiteralExpr : Expr {
    Token token; // holds type/value (INT, FLOAT, STRING, BOOL)
    LiteralExpr(const Token &t) : token(t) {}
};

struct VariableExpr : Expr {
    std::string name;
    VariableExpr(const std::string &n) : name(n) {}
};

struct UnaryExpr : Expr {
    Token op;
    std::unique_ptr<Expr> right;
    UnaryExpr(const Token &o, std::unique_ptr<Expr> r)
        : op(o), right(std::move(r)) {}
};

struct BinaryExpr : Expr {
    std::unique_ptr<Expr> left;
    Token op;
    std::unique_ptr<Expr> right;
    BinaryExpr(std::unique_ptr<Expr> l, const Token &o, std::unique_ptr<Expr> r)
        : left(std::move(l)), op(o), right(std::move(r)) {}
};

struct CallExpr : Expr {
    std::string callee;
    std::vector<std::unique_ptr<Expr>> args;
    CallExpr(const std::string &c) : callee(c) {}
};

// ----- Statements -----
struct ExprStmt : Stmt {
    std::unique_ptr<Expr> expr;
    ExprStmt(std::unique_ptr<Expr> e) : expr(std::move(e)) {}
};

struct LetStmt : Stmt {
    std::string name;
    std::unique_ptr<Expr> init; // may be null
    LetStmt(const std::string &n, std::unique_ptr<Expr> i)
        : name(n), init(std::move(i)) {}
};

struct VarStmt : Stmt {
    std::string name;
    std::unique_ptr<Expr> init; // may be null
    VarStmt(const std::string &n, std::unique_ptr<Expr> i)
        : name(n), init(std::move(i)) {}
};

struct BlockStmt : Stmt {
    std::vector<std::unique_ptr<Stmt>> statements;
};

struct IfStmt : Stmt {
    std::unique_ptr<Expr> condition;
    std::unique_ptr<BlockStmt> thenBranch;
    std::unique_ptr<Stmt> elseBranch; // can be BlockStmt or IfStmt
};
