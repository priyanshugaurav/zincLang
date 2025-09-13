#include "parser.h"
#include <stdexcept>
#include <iostream>

Parser::Parser(const std::vector<Token> &tokens) : tokens(tokens) {}

const Token &Parser::peekNext() const
{
    if (current + 1 < tokens.size())
        return tokens[current + 1];
    return tokens.back(); // safe fallback
}

std::vector<StmtPtr> Parser::parseProgram()
{
    std::vector<StmtPtr> statements;
    while (!isAtEnd())
    {
        statements.push_back(declaration());
    }
    return statements;
}

StmtPtr Parser::parse()
{
    std::vector<StmtPtr> stmts;
    for (auto s : parseProgram())
    {
        if (s)
            stmts.push_back(s);
    }
    return std::make_shared<BlockStmt>(stmts);
}

// -------------------
// Helpers
// -------------------
const Token &Parser::peek() const { return tokens[current]; }
const Token &Parser::previous() const { return tokens[current - 1]; }
bool Parser::isAtEnd() const { return peek().type == TokenType::Eof; }

bool Parser::check(TokenType type) const
{
    if (isAtEnd())
        return false;
    return peek().type == type;
}

const Token &Parser::advance()
{
    if (!isAtEnd())
        current++;
    return previous();
}

bool Parser::match(const std::vector<TokenType> &types)
{
    for (auto t : types)
    {
        if (check(t))
        {
            advance();
            return true;
        }
    }
    return false;
}

void Parser::consume(TokenType type, const std::string &msg)
{
    if (check(type))
    {
        advance();
        return;
    }
    throw std::runtime_error("Parse error at line " + std::to_string(peek().line) + ": " + msg);
}

// -------------------
// Declarations
// -------------------
StmtPtr Parser::declaration()
{
    try
    {
        if (match({TokenType::Fn}))
            return functionDeclaration();
        if (match({TokenType::Let, TokenType::Var}))
            return varDeclaration();
        return statement();
    }
    catch (const std::runtime_error &e)
    {
        std::cerr << "Parse error: " << e.what() << " at token: "
                  << peek().value << "\n";
        synchronize();
        return nullptr;
    }
}

// simple prototype
std::string Parser::parseType()
{
    consume(TokenType::Identifier, "Expected type name");
    std::string typeName = previous().value;

    // Check for generic: e.g., array<...>
    if (match({TokenType::Less}))
    {
        std::string innerType = parseType();  // recursively parse
        consume(TokenType::Greater, "Expected '>' after generic type");
        typeName += "<" + innerType + ">";
    }

    // Check for nullable type: e.g., float?
    while (match({TokenType::Question}))
    {
        typeName += "?";
    }

    return typeName;
}


StmtPtr Parser::varDeclaration()
{
    bool isMutable = previous().type == TokenType::Var;
    Token name = peek();
    consume(TokenType::Identifier, "Expected variable name.");

    std::string typeHint;
    if (match({TokenType::Colon}))
    {
        typeHint = parseType();
    }

    ExprPtr initializer = nullptr;
    if (match({TokenType::Assign}))
    {
        initializer = expression();
    }

    // optional semicolon
    if (check(TokenType::Semicolon))
        advance();

    return std::make_shared<VarDecl>(isMutable, name.value, initializer, typeHint);
}

StmtPtr Parser::functionDeclaration()
{
    Token name = peek();
    consume(TokenType::Identifier, "Expected function name.");
    consume(TokenType::LParen, "Expected '(' after function name.");
    std::vector<std::pair<std::string, std::string>> params;

    if (!check(TokenType::RParen))
    {
        do
        {
            Token paramName = peek();
            consume(TokenType::Identifier, "Expected parameter name.");

            // make ':' optional: if provided, parse the type, otherwise leave empty (dynamic)
            std::string paramType;
            if (match({TokenType::Colon}))
            {
                paramType = parseType();
            }
            else
            {
                paramType = ""; // empty string => dynamic / any (handled by TypeChecker)
            }

            params.emplace_back(paramName.value, paramType);

        } while (match({TokenType::Comma}));
    }

    consume(TokenType::RParen, "Expected ')' after parameters.");

    // parse optional return type using parseType() to support generics/nullability
    std::string returnType;
    if (match({TokenType::Colon}))
    {
        returnType = parseType();
    }

    StmtPtr body = blockStatement();
    return std::make_shared<FuncDecl>(name.value, params, body, returnType);
}



// Statements
// -------------------
StmtPtr Parser::statement()
{
    if (match({TokenType::If}))
        return ifStatement();
    if (match({TokenType::While}))
        return whileStatement();
    if (match({TokenType::For}))
        return forStatement();
    if (match({TokenType::Times}))
        return timesStatement();
    if (match({TokenType::Return}))
        return returnStatement();

    if (match({TokenType::LBrace}))
        return blockStatementAlreadyConsumed();

    return expressionStatement();
}

StmtPtr Parser::ifStatement()
{
    // No need to consume 'if' here because it's already matched in `statement()`

    ExprPtr condition = expression(); // parse condition

    StmtPtr thenBranch = nullptr;
    if (check(TokenType::LBrace))
    {
        thenBranch = blockStatement(); // parse { ... } block
    }
    else
    {
        thenBranch = statement(); // single statement
    }

    StmtPtr elseBranch = nullptr;
    if (match({TokenType::Else}))
    {
        if (match({TokenType::If}))
        {
            elseBranch = ifStatement(); // recursion
        }
        else if (check(TokenType::LBrace))
        {
            elseBranch = blockStatement(); // else { ... }
        }
        else
        {
            elseBranch = statement(); // else single stmt
        }
    }

    return std::make_shared<IfStmt>(condition, thenBranch, elseBranch);
}

// Standard block: consumes '{'
StmtPtr Parser::blockStatement()
{
    consume(TokenType::LBrace, "Expected '{' to start block");
    return finishBlock();
}

// Variant: called when '{' was already consumed in statement()
StmtPtr Parser::blockStatementAlreadyConsumed()
{
    return finishBlock();
}

StmtPtr Parser::finishBlock()
{
    std::vector<StmtPtr> statements;
    while (!check(TokenType::RBrace) && !isAtEnd())
    {
        StmtPtr stmt = nullptr;
        try
        {
            if (check(TokenType::Let) || check(TokenType::Var) || check(TokenType::Fn))
                stmt = declaration();
            else
                stmt = statement();
        }
        catch (std::runtime_error &e)
        {
            std::cerr << "Parse error: " << e.what() << " at token: " << peek().value << "\n";
            synchronize();
        }
        if (stmt)
            statements.push_back(stmt);
    }
    consume(TokenType::RBrace, "Expected '}' after block");
    return std::make_shared<BlockStmt>(statements);
}

ExprPtr Parser::ifExpression()
{
    consume(TokenType::If, "Expected 'if'");
    ExprPtr condition = expression();

    ExprPtr thenBranch;
    if (check(TokenType::LBrace))
        thenBranch = blockExpression();
    else
        thenBranch = expression();

    ExprPtr elseBranch = nullptr;
    if (match({TokenType::Else}))
    {
        if (check(TokenType::If))
            elseBranch = ifExpression();
        else if (check(TokenType::LBrace))
            elseBranch = blockExpression();
        else
            elseBranch = expression();
    }

    return std::make_shared<IfExpr>(condition, thenBranch, elseBranch);
}

StmtPtr Parser::whileStatement()
{
    // 'while' already matched in statement()
    ExprPtr cond = expression();
    StmtPtr body = blockStatement();
    return std::make_shared<WhileStmt>(cond, body);
}

StmtPtr Parser::forStatement()
{
    // 'for' already matched in statement()
    Token iter = peek();
    consume(TokenType::Identifier, "Expected iterator variable.");
    consume(TokenType::In, "Expected 'in' keyword.");
    ExprPtr iterable = expression();
    StmtPtr body = blockStatement();
    return std::make_shared<ForStmt>(iter.value, iterable, body);
}

StmtPtr Parser::timesStatement()
{
    ExprPtr count;

    if (match({TokenType::LParen}))
    {
        // form: times(expr) { ... }
        count = expression();
        consume(TokenType::RParen, "Expected ')' after times count");
    }
    else
    {
        // form: times expr { ... }
        count = expression();
    }

    StmtPtr body = blockStatement();
    return std::make_shared<TimesStmt>(count, body);
}

StmtPtr Parser::returnStatement()
{
    ExprPtr value = nullptr;
    if (!check(TokenType::Semicolon))
        value = expression();
    return std::make_shared<ReturnStmt>(value);
}

ExprPtr Parser::blockExpression()
{
    consume(TokenType::LBrace, "Expected '{' for block");

    std::vector<StmtPtr> statements;
    while (!check(TokenType::RBrace) && !isAtEnd())
    {
        if (check(TokenType::Let) || check(TokenType::Var))
            statements.push_back(declaration());
        else
            statements.push_back(expressionStatement());
    }

    consume(TokenType::RBrace, "Expected '}' after block");

    // last statement must be expression
    if (statements.empty())
        throw std::runtime_error("Block must not be empty");

    StmtPtr last = statements.back();

    if (auto exprStmt = std::dynamic_pointer_cast<ExprStmt>(last))
        return exprStmt->expr;
    if (auto varDecl = std::dynamic_pointer_cast<VarDecl>(last))
        if (varDecl->initializer)
            return varDecl->initializer;

    // Wrap entire block as an "anonymous block expression" if needed
    std::vector<ExprPtr> exprs;
    for (auto &stmt : statements)
    {
        if (auto eStmt = std::dynamic_pointer_cast<ExprStmt>(stmt))
            exprs.push_back(eStmt->expr);
    }
    if (!exprs.empty())
        return exprs.back(); // take last expression

    throw std::runtime_error("Block must end with an expression");
}

StmtPtr Parser::expressionStatement()
{
    ExprPtr expr;
    try
    {
        expr = expression();
    }
    catch (std::runtime_error &e)
    {
        std::cerr << "Parse error: " << e.what() << " at token: " << peek().value << "\n";
        synchronize();
        return nullptr;
    }

    if (check(TokenType::Semicolon))
        advance();
    return std::make_shared<ExprStmt>(expr);
}

// -------------------
// Expressions
// -------------------
ExprPtr Parser::expression()
{
    if (check(TokenType::If))
        return ifExpression(); // consumes 'if' inside ifExpression
    return assignment();
}

ExprPtr Parser::assignment()
{
    ExprPtr expr = logicalOr();

    if (match({TokenType::Assign}))
    {
        if (auto id = std::dynamic_pointer_cast<IdentifierExpr>(expr))
        {
            ExprPtr value = expression(); // use expression() here to allow if-expr
            return std::make_shared<BinaryExpr>(expr, "=", value);
        }

        if (auto idx = std::dynamic_pointer_cast<IndexExpr>(expr))
        {
            ExprPtr value = expression(); // same here
            return std::make_shared<ArrayAssignExpr>(idx->array, idx->index, value);
        }

        throw std::runtime_error("Invalid assignment target.");
    }

    return expr;
}

ExprPtr Parser::logicalOr()
{
    ExprPtr expr = logicalAnd();
    while (match({TokenType::OrOr}))
    {
        Token op = previous();
        ExprPtr right = logicalAnd();
        expr = std::make_shared<BinaryExpr>(expr, op.value, right);
    }
    return expr;
}

ExprPtr Parser::logicalAnd()
{
    ExprPtr expr = equality();
    while (match({TokenType::AndAnd}))
    {
        Token op = previous();
        ExprPtr right = equality();
        expr = std::make_shared<BinaryExpr>(expr, op.value, right);
    }
    return expr;
}

ExprPtr Parser::equality()
{
    ExprPtr expr = comparison();
    while (match({TokenType::Equal, TokenType::NotEqual}))
    {
        Token op = previous();
        ExprPtr right = comparison();
        expr = std::make_shared<BinaryExpr>(expr, op.value, right);
    }
    return expr;
}

ExprPtr Parser::comparison()
{
    ExprPtr expr = term();
    while (match({TokenType::Less, TokenType::LessEqual, TokenType::Greater, TokenType::GreaterEqual}))
    {
        Token op = previous();
        ExprPtr right = term();
        expr = std::make_shared<BinaryExpr>(expr, op.value, right);
    }
    return expr;
}

ExprPtr Parser::term()
{
    ExprPtr expr = factor();
    while (match({TokenType::Plus, TokenType::Minus}))
    {
        Token op = previous();
        ExprPtr right = factor();
        expr = std::make_shared<BinaryExpr>(expr, op.value, right);
    }
    return expr;
}

ExprPtr Parser::factor()
{
    ExprPtr expr = unary();
    while (match({TokenType::Star, TokenType::Slash, TokenType::MOD}))
    {
        Token op = previous();
        ExprPtr right = unary();
        expr = std::make_shared<BinaryExpr>(expr, op.value, right);
    }
    return expr;
}

ExprPtr Parser::unary()
{
    if (match({TokenType::Bang, TokenType::Minus}))
    {
        Token op = previous();
        ExprPtr right = unary();
        return std::make_shared<UnaryExpr>(op.value, right);
    }
    return call();
}

ExprPtr Parser::call()
{
    ExprPtr expr = primary();
    while (true)
    {
        if (match({TokenType::LParen}))
        {
            expr = finishCall(expr);
        }
        else if (match({TokenType::LBracket}))
        {
            expr = indexExpr(expr);
        }
        else
        {
            break;
        }
    }
    return expr;
}

ExprPtr Parser::finishCall(ExprPtr callee)
{
    std::vector<ExprPtr> args;
    if (!check(TokenType::RParen))
    {
        do
        {
            args.push_back(expression());
        } while (match({TokenType::Comma}));
    }
    consume(TokenType::RParen, "Expected ')' after arguments.");
    return std::make_shared<CallExpr>(callee, args);
}

ExprPtr Parser::indexExpr(ExprPtr array)
{
    ExprPtr idx = expression();
    consume(TokenType::RBracket, "Expected ']' after index.");
    return std::make_shared<IndexExpr>(array, idx);
}

ExprPtr Parser::primary()
{
    if (match({TokenType::Number}))
    {
        std::string val = previous().value;
        std::string type = (val.find('.') != std::string::npos) ? "float" : "int";
        return std::make_shared<LiteralExpr>(val, type);
    }

    if (match({TokenType::String}))
        return std::make_shared<LiteralExpr>(previous().value, "string");

    if (match({TokenType::True, TokenType::False}))
        return std::make_shared<LiteralExpr>(previous().value, "bool");

    if (match({TokenType::Null})) // ✅ Add this
        return std::make_shared<LiteralExpr>("null", "null");

    if (match({TokenType::Identifier}))
        return std::make_shared<IdentifierExpr>(previous().value);

    if (match({TokenType::LParen}))
    {
        ExprPtr expr = expression();
        consume(TokenType::RParen, "Expected ')' after expression.");
        return expr;
    }

    if (match({TokenType::LBracket}))
        return arrayLiteral();

    throw std::runtime_error("Expected expression at line " + std::to_string(peek().line));
}

ExprPtr Parser::arrayLiteral()
{
    std::vector<ExprPtr> elems;
    if (!check(TokenType::RBracket))
    {
        do
        {
            elems.push_back(expression());
        } while (match({TokenType::Comma}));
    }
    consume(TokenType::RBracket, "Expected ']' after array literal.");
    return std::make_shared<ArrayExpr>(elems);
}

// -------------------
// Error recovery
// -------------------
void Parser::synchronize()
{
    advance();
    while (!isAtEnd())
    {
        if (previous().type == TokenType::Semicolon)
            return;

        switch (peek().type)
        {
        case TokenType::Fn:
        case TokenType::Let:
        case TokenType::Var:
        case TokenType::For:
        case TokenType::If:
        case TokenType::While:
        case TokenType::Return:
            return;
        default:
            break;
        }
        advance();
    }
}
