#pragma once
#include <string>
#include <vector>
#include <cctype>
#include <stdexcept>

enum class TokenType
{
    // Single-character tokens
    PLUS,
    MINUS,
    STAR,
    SLASH,
    PERCENT,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    COMMA,
    SEMICOLON,
    ASSIGN,

    // Comparison
    EQ,
    NEQ,
    LT,
    GT,
    LTE,
    GTE,

    // Logical
    AND,
    OR,
    NOT,

    // Literals
    INT,
    FLOAT,
    STRING,
    BOOL,
    IDENTIFIER,

    // Keywords
    LET,
    VAR,
    FN,
    IF,
    ELSE,
    WHILE,
    FOR,
    TIMES,
    RETURN,
    NULLVAL,

    END_OF_FILE,
};

struct Token
{
    TokenType type;
    std::string value;
    int line;

    Token(TokenType t, const std::string &val, int ln)
        : type(t), value(val), line(ln) {}
};

class Lexer
{
private:
    std::string source;
    size_t pos;
    int line;
    bool isAtEnd() const;
    char currentChar() const;

    void advance();
    void skipWhitespace();
    Token number();
    Token identifier();
    Token stringLiteral();
    bool isKeyword(const std::string &str, TokenType &type);

public:
    Lexer(const std::string &src);
    Token nextToken();
};
