#pragma once
#include <string>
#include <vector>

enum class TokenType {
    // Special
    Eof,

    // Literals
    Identifier,
    Number,     // integer or float (lexed as text)
    String,
    Null, 

    // Keywords
    Let,
    Var,
    Fn,
    If,
    Else,
    Then,
    While,
    Return,
    For,
    In,
    Times,
    True,
    False,
    Question,

    // Punctuation
    LParen, RParen,
    LBrace, RBrace,
    LBracket, RBracket,
    Colon, Semicolon, Comma,

    // Operators
    Plus, Minus, Star, Slash, MOD,
    Bang, NotEqual,
    Assign, Equal,
    AndAnd, OrOr,
    BitAnd, BitOr, BitXor,
    ShiftLeft, ShiftRight,
    Less, LessEqual, Greater, GreaterEqual,

    // Other
    Unknown
};

struct Token {
    TokenType type;
    std::string value; // the raw lexeme (if applicable)
    int line = 0;
    int col = 0;
};

std::vector<Token> lexString(const std::string &src);
