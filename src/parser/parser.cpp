#include "parser.h"
#include <stdexcept>
#include <iostream>
#include <sstream>

Parser::Parser(const std::vector<Token> &toks) : tokens(toks), current(0) {}

Token Parser::peek() const {
    if (current >= tokens.size()) return tokens.back();
    return tokens[current];
}

Token Parser::previous() const {
    if (current == 0) return tokens.front();
    return tokens[current - 1];
}

Token Parser::advance() {
    if (!isAtEnd()) current++;
    return previous();
}

bool Parser::isAtEnd() const {
    return peek().type == TokenType::END_OF_FILE;
}

bool Parser::check(TokenType t) const {
    if (isAtEnd()) return false;
    return peek().type == t;
}

bool Parser::match(TokenType t) {
    if (check(t)) {
        advance();
        return true;
    }
    return false;
}

void Parser::consume(TokenType t, const std::string &errMsg) {
    if (check(t)) {
        advance();
        return;
    }
    std::ostringstream ss;
    ss << errMsg << " (line " << peek().line << ")";
    throw std::runtime_error(ss.str());
}

// ------------------- Expressions (precedence) -------------------
std::unique_ptr<Expr> Parser::expression() {
    return orExpr();
}

std::unique_ptr<Expr> Parser::orExpr() {
    auto expr = andExpr();
    while (match(TokenType::OR)) {
        Token op = previous();
        auto right = andExpr();
        expr = std::make_unique<BinaryExpr>(std::move(expr), op, std::move(right));
    }
    return expr;
}

std::unique_ptr<Expr> Parser::andExpr() {
    auto expr = equality();
    while (match(TokenType::AND)) {
        Token op = previous();
        auto right = equality();
        expr = std::make_unique<BinaryExpr>(std::move(expr), op, std::move(right));
    }
    return expr;
}

std::unique_ptr<Expr> Parser::equality() {
    auto expr = comparison();
    while (match(TokenType::EQ) || match(TokenType::NEQ)) {
        Token op = previous();
        auto right = comparison();
        expr = std::make_unique<BinaryExpr>(std::move(expr), op, std::move(right));
    }
    return expr;
}

std::unique_ptr<Expr> Parser::comparison() {
    auto expr = term();
    while (match(TokenType::LT) || match(TokenType::LTE) ||
           match(TokenType::GT) || match(TokenType::GTE)) {
        Token op = previous();
        auto right = term();
        expr = std::make_unique<BinaryExpr>(std::move(expr), op, std::move(right));
    }
    return expr;
}

std::unique_ptr<Expr> Parser::term() {
    auto expr = factor();
    while (match(TokenType::PLUS) || match(TokenType::MINUS)) {
        Token op = previous();
        auto right = factor();
        expr = std::make_unique<BinaryExpr>(std::move(expr), op, std::move(right));
    }
    return expr;
}

std::unique_ptr<Expr> Parser::factor() {
    auto expr = unary();
    while (match(TokenType::STAR) || match(TokenType::SLASH) || match(TokenType::PERCENT)) {
        Token op = previous();
        auto right = unary();
        expr = std::make_unique<BinaryExpr>(std::move(expr), op, std::move(right));
    }
    return expr;
}

std::unique_ptr<Expr> Parser::unary() {
    if (match(TokenType::NOT) || match(TokenType::MINUS)) {
        Token op = previous();
        auto right = unary();
        return std::make_unique<UnaryExpr>(op, std::move(right));
    }
    return call();
}

// call handles function calls (identifier '(' args ')')
std::unique_ptr<Expr> Parser::call() {
    auto expr = primary();

    // If primary returned a VariableExpr and it's followed by LPAREN => call
    while (true) {
        if (match(TokenType::LPAREN)) {
            // previous() is '('; but callee is in expr (should be VariableExpr)
            // We'll assume callee name exists when expr is VariableExpr
            std::string calleeName;
            if (auto *v = dynamic_cast<VariableExpr*>(expr.get())) {
                calleeName = v->name;
            } else {
                throw std::runtime_error("Call target must be an identifier (line " + std::to_string(previous().line) + ")");
            }

            auto callExpr = std::make_unique<CallExpr>(calleeName);

            // parse zero or more args until RPAREN
            if (!check(TokenType::RPAREN)) {
                do {
                    callExpr->args.push_back(expression());
                } while (match(TokenType::COMMA));
            }
            consume(TokenType::RPAREN, "Expected ')' after arguments");

            // expr becomes the call expression
            expr = std::move(callExpr);
            continue;
        }
        break;
    }
    return expr;
}

std::unique_ptr<Expr> Parser::primary() {
    if (match(TokenType::INT) || match(TokenType::FLOAT) || match(TokenType::STRING) || match(TokenType::BOOL)) {
        return std::make_unique<LiteralExpr>(previous());
    }
    if (match(TokenType::IDENTIFIER)) {
        return std::make_unique<VariableExpr>(previous().value);
    }
    if (match(TokenType::LPAREN)) {
        auto expr = expression();
        consume(TokenType::RPAREN, "Expected ')' after expression");
        return expr;
    }
    std::ostringstream ss;
    ss << "Unexpected token in expression: '" << peek().value << "' (line " << peek().line << ")";
    throw std::runtime_error(ss.str());
}

// ------------------- Statements -------------------

std::unique_ptr<Stmt> Parser::declaration() {
    if (match(TokenType::LET)) return letDeclaration();
    if (match(TokenType::VAR)) return varDeclaration();
    return statement();
}

std::unique_ptr<Stmt> Parser::letDeclaration() {
    if (!check(TokenType::IDENTIFIER)) throw std::runtime_error("Expected identifier after 'let'");
    Token nameTok = advance(); // consume identifier
    std::unique_ptr<Expr> initializer = nullptr;
    if (match(TokenType::ASSIGN)) {
        initializer = expression();
    }
    return std::make_unique<LetStmt>(nameTok.value, std::move(initializer));
}

std::unique_ptr<Stmt> Parser::varDeclaration() {
    if (!check(TokenType::IDENTIFIER)) throw std::runtime_error("Expected identifier after 'var'");
    Token nameTok = advance(); // consume identifier
    std::unique_ptr<Expr> initializer = nullptr;
    if (match(TokenType::ASSIGN)) {
        initializer = expression();
    }
    return std::make_unique<VarStmt>(nameTok.value, std::move(initializer));
}

std::unique_ptr<BlockStmt> Parser::block() {
    auto block = std::make_unique<BlockStmt>();
    // assume '{' was already consumed by caller
    while (!check(TokenType::RBRACE) && !isAtEnd()) {
        block->statements.push_back(declaration());
    }
    consume(TokenType::RBRACE, "Expected '}' after block");
    return block;
}

std::unique_ptr<Stmt> Parser::ifStatement() {
    // 'if' already consumed in caller (declaration/statement)
    // Accept optional '(' condition ')' or direct condition (we'll allow parentheses)
    if (match(TokenType::LPAREN)) {
        // parsed '(' earlier - parse condition then expect ')'
        auto cond = expression();
        consume(TokenType::RPAREN, "Expected ')' after if condition");
        // then block
        if (!match(TokenType::LBRACE)) throw std::runtime_error("Expected '{' after if condition");
        auto thenBlk = block();
        std::unique_ptr<Stmt> elseBranch = nullptr;
        if (match(TokenType::ELSE)) {
            if (match(TokenType::IF)) {
                elseBranch = ifStatement(); // nested if
            } else if (match(TokenType::LBRACE)) {
                elseBranch = block();
            } else {
                throw std::runtime_error("Expected 'if' or '{' after else");
            }
        }
        auto stmt = std::make_unique<IfStmt>();
        stmt->condition = std::move(cond);
        stmt->thenBranch = std::move(thenBlk);
        stmt->elseBranch = std::move(elseBranch);
        return stmt;
    } else {
        // Support if without parentheses: parse condition directly
        auto cond = expression();
        if (!match(TokenType::LBRACE)) throw std::runtime_error("Expected '{' after if condition");
        auto thenBlk = block();
        std::unique_ptr<Stmt> elseBranch = nullptr;
        if (match(TokenType::ELSE)) {
            if (match(TokenType::IF)) {
                elseBranch = ifStatement();
            } else if (match(TokenType::LBRACE)) {
                elseBranch = block();
            } else {
                throw std::runtime_error("Expected 'if' or '{' after else");
            }
        }
        auto stmt = std::make_unique<IfStmt>();
        stmt->condition = std::move(cond);
        stmt->thenBranch = std::move(thenBlk);
        stmt->elseBranch = std::move(elseBranch);
        return stmt;
    }
}

std::unique_ptr<Stmt> Parser::exprStatement() {
    auto e = expression();
    // we do not require semicolons — statements are terminated by newlines/blocks in your examples
    return std::make_unique<ExprStmt>(std::move(e));
}

std::unique_ptr<Stmt> Parser::statement() {
    if (match(TokenType::IF)) return ifStatement();
    if (match(TokenType::LBRACE)) return block();
    return exprStatement();
}

// ------------------- Top-level parse -------------------
std::vector<std::unique_ptr<Stmt>> Parser::parse() {
    std::vector<std::unique_ptr<Stmt>> program;
    while (!isAtEnd()) {
        program.push_back(declaration());
    }
    return program;
}

// ------------------- AST printer (debug) -------------------
static void indentPrint(int n) {
    for (int i = 0; i < n; ++i) std::cout << "  ";
}

void printExpr(const Expr *e, int indent);

void printStmt(const Stmt *s, int indent) {
    if (!s) return;
    if (auto *ls = dynamic_cast<const LetStmt*>(s)) {
        indentPrint(indent); std::cout << "Let " << ls->name << "\n";
        if (ls->init) { indentPrint(indent+1); std::cout << "Init:\n"; printExpr(ls->init.get(), indent+2); }
    } else if (auto *vs = dynamic_cast<const VarStmt*>(s)) {
        indentPrint(indent); std::cout << "Var " << vs->name << "\n";
        if (vs->init) { indentPrint(indent+1); std::cout << "Init:\n"; printExpr(vs->init.get(), indent+2); }
    } else if (auto *es = dynamic_cast<const ExprStmt*>(s)) {
        indentPrint(indent); std::cout << "ExprStmt\n"; printExpr(es->expr.get(), indent+1);
    } else if (auto *bs = dynamic_cast<const BlockStmt*>(s)) {
        indentPrint(indent); std::cout << "Block\n";
        for (auto &st : bs->statements) printStmt(st.get(), indent+1);
    } else if (auto *ifs = dynamic_cast<const IfStmt*>(s)) {
        indentPrint(indent); std::cout << "If\n";
        indentPrint(indent+1); std::cout << "Cond:\n"; printExpr(ifs->condition.get(), indent+2);
        indentPrint(indent+1); std::cout << "Then:\n"; printStmt(ifs->thenBranch.get(), indent+2);
        if (ifs->elseBranch) { indentPrint(indent+1); std::cout << "Else:\n"; printStmt(ifs->elseBranch.get(), indent+2); }
    } else {
        indentPrint(indent); std::cout << "Unknown Stmt\n";
    }
}

void printExpr(const Expr *e, int indent) {
    if (!e) return;
    if (auto *lit = dynamic_cast<const LiteralExpr*>(e)) {
        indentPrint(indent); std::cout << "Literal(" << lit->token.value << ")\n";
    } else if (auto *var = dynamic_cast<const VariableExpr*>(e)) {
        indentPrint(indent); std::cout << "Variable(" << var->name << ")\n";
    } else if (auto *un = dynamic_cast<const UnaryExpr*>(e)) {
        indentPrint(indent); std::cout << "Unary(" << un->op.value << ")\n";
        printExpr(un->right.get(), indent+1);
    } else if (auto *bin = dynamic_cast<const BinaryExpr*>(e)) {
        indentPrint(indent); std::cout << "Binary(" << bin->op.value << ")\n";
        printExpr(bin->left.get(), indent+1);
        printExpr(bin->right.get(), indent+1);
    } else if (auto *call = dynamic_cast<const CallExpr*>(e)) {
        indentPrint(indent); std::cout << "Call(" << call->callee << ")\n";
        for (auto &a : call->args) printExpr(a.get(), indent+1);
    } else {
        indentPrint(indent); std::cout << "Unknown Expr\n";
    }
}

void Parser::printAST(const std::vector<std::unique_ptr<Stmt>> &program, int indent) {
    for (auto &s : program) printStmt(s.get(), indent);
}
