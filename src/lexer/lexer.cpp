#include "lexer.h"

Lexer::Lexer(const std::string &src) : source(src), pos(0), line(1) {}

bool Lexer::isAtEnd() const {
    return pos >= source.size();
}

char Lexer::currentChar() const {
    if (isAtEnd()) return '\0';
    return source[pos];
}


void Lexer::advance()
{
    if (currentChar() == '\n')
        line++;
    pos++;
}

void Lexer::skipWhitespace()
{
    while (isspace(currentChar()))
        advance();
}

bool Lexer::isKeyword(const std::string &str, TokenType &type)
{
    if (str == "let")
        type = TokenType::LET;
    else if (str == "var")
        type = TokenType::VAR;
    else if (str == "fn")
        type = TokenType::FN;
    else if (str == "if")
        type = TokenType::IF;
    else if (str == "else")
        type = TokenType::ELSE;
    else if (str == "while")
        type = TokenType::WHILE;
    else if (str == "for")
        type = TokenType::FOR;
    else if (str == "times")
        type = TokenType::TIMES;
    else if (str == "return")
        type = TokenType::RETURN;
    else if (str == "null")
        type = TokenType::NULLVAL;
    else
        return false;
    return true;
}

Token Lexer::number()
{
    std::string result;
    bool hasDot = false;
    while (isdigit(currentChar()) || currentChar() == '.')
    {
        if (currentChar() == '.')
        {
            if (hasDot)
                break;
            hasDot = true;
        }
        result += currentChar();
        advance();
    }
    return hasDot ? Token(TokenType::FLOAT, result, line)
                  : Token(TokenType::INT, result, line);
}

Token Lexer::identifier()
{
    std::string result;
    while (isalnum(currentChar()) || currentChar() == '_')
    {
        result += currentChar();
        advance();
    }
    TokenType type;
    if (isKeyword(result, type))
        return Token(type, result, line);
    return Token(TokenType::IDENTIFIER, result, line);
}

Token Lexer::stringLiteral()
{
    advance(); // consume opening "

    size_t start = pos;
    while (!isAtEnd() && currentChar() != '"')
    {
        advance();
    }
    if (isAtEnd())
    {
        throw std::runtime_error("Unterminated string literal");
    }

    std::string value = source.substr(start, pos - start);
    advance(); // consume closing "

    return Token(TokenType::STRING, value, line);
}

Token Lexer::nextToken()
{
    skipWhitespace();

    char ch = currentChar();
    if (ch == '\0')
        return Token(TokenType::END_OF_FILE, "", line);

    // Numbers
    if (isdigit(ch))
        return number();

    // Identifiers / keywords / booleans
    if (isalpha(ch) || ch == '_')
    {
        Token t = identifier();
        if (t.value == "true" || t.value == "false")
        {
            return Token(TokenType::BOOL, t.value, line);
        }
        return t;
    }

    // Strings
    if (ch == '"')
        return stringLiteral();

    // Comments
    if (ch == '/')
    {
        if (pos + 1 < source.size() && source[pos + 1] == '/')
        {
            while (currentChar() != '\n' && currentChar() != '\0')
                advance();
            return nextToken();
        }
        if (pos + 1 < source.size() && source[pos + 1] == '*')
        {
            advance(); // /
            advance(); // *
            while (!(currentChar() == '*' && pos + 1 < source.size() && source[pos + 1] == '/'))
            {
                if (currentChar() == '\0')
                    throw std::runtime_error("Unterminated block comment");
                advance();
            }
            advance(); // *
            advance(); // /
            return nextToken();
        }
    }

    // Operators & symbols
    switch (ch)
    {
    case '+':
        advance();
        return Token(TokenType::PLUS, "+", line);
    case '-':
        advance();
        return Token(TokenType::MINUS, "-", line);
    case '*':
        advance();
        return Token(TokenType::STAR, "*", line);
    case '/':
        advance();
        return Token(TokenType::SLASH, "/", line);
    case '%':
        advance();
        return Token(TokenType::PERCENT, "%", line);

    case '(':
        advance();
        return Token(TokenType::LPAREN, "(", line);
    case ')':
        advance();
        return Token(TokenType::RPAREN, ")", line);
    case '{':
        advance();
        return Token(TokenType::LBRACE, "{", line);
    case '}':
        advance();
        return Token(TokenType::RBRACE, "}", line);
    case '[':
        advance();
        return Token(TokenType::LBRACKET, "[", line);
    case ']':
        advance();
        return Token(TokenType::RBRACKET, "]", line);
    case ',':
        advance();
        return Token(TokenType::COMMA, ",", line);
    case ';':
        advance();
        return Token(TokenType::SEMICOLON, ";", line);

    case '=':
        advance();
        if (currentChar() == '=')
        {
            advance();
            return Token(TokenType::EQ, "==", line);
        }
        return Token(TokenType::ASSIGN, "=", line);

    case '!':
        advance();
        if (currentChar() == '=')
        {
            advance();
            return Token(TokenType::NEQ, "!=", line);
        }
        return Token(TokenType::NOT, "!", line);

    case '<':
        advance();
        if (currentChar() == '=')
        {
            advance();
            return Token(TokenType::LTE, "<=", line);
        }
        return Token(TokenType::LT, "<", line);

    case '>':
        advance();
        if (currentChar() == '=')
        {
            advance();
            return Token(TokenType::GTE, ">=", line);
        }
        return Token(TokenType::GT, ">", line);

    case '&':
        if (pos + 1 < source.size() && source[pos + 1] == '&')
        {
            advance();
            advance();
            return Token(TokenType::AND, "&&", line);
        }
        break;

    case '|':
        if (pos + 1 < source.size() && source[pos + 1] == '|')
        {
            advance();
            advance();
            return Token(TokenType::OR, "||", line);
        }
        break;
    }

    throw std::runtime_error("Unknown character at line " + std::to_string(line) + ": " + ch);
}
