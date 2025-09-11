#include "lexer.h"
#include <string>
#include <vector>
#include <unordered_map>
#include <cctype>
#include <stdexcept>

class Lexer
{
    std::string src;
    size_t pos = 0;
    int line = 1;
    int col = 1;

    std::unordered_map<std::string, TokenType> keywords = {
        {"let", TokenType::Let},
        {"var", TokenType::Var},
        {"fn", TokenType::Fn},
        {"if", TokenType::If},
        {"else", TokenType::Else},
        {"while", TokenType::While},
        {"return", TokenType::Return},
        {"for", TokenType::For},
        {"in", TokenType::In},
        {"times", TokenType::Times},
        {"true", TokenType::True},
        {"false", TokenType::False},
        {"null", TokenType::Null},
    };

    char peek() const { return pos < src.size() ? src[pos] : '\0'; }
    char peekNext() const { return (pos + 1) < src.size() ? src[pos + 1] : '\0'; }

    char advance()
    {
        char c = peek();
        if (c == '\0')
            return c;
        if (c == '\n')
        {
            line++;
            col = 1;
        }
        else
        {
            col++;
        }
        pos++;
        return c;
    }

    bool match(char expected)
    {
        if (peek() == expected)
        {
            advance();
            return true;
        }
        return false;
    }

    void skipWhitespaceAndComments()
    {
        while (true)
        {
            char c = peek();
            // whitespace
            if (c == ' ' || c == '\t' || c == '\r' || c == '\n')
            {
                advance();
                continue;
            }

            // single-line comment //
            if (c == '/' && peekNext() == '/')
            {
                advance(); // '/'
                advance(); // '/'
                while (peek() != '\n' && peek() != '\0')
                    advance();
                continue;
            }

            // block comment /* ... */
            if (c == '/' && peekNext() == '*')
            {
                advance(); // '/'
                advance(); // '*'
                while (!(peek() == '*' && peekNext() == '/'))
                {
                    if (peek() == '\0')
                    {
                        throw std::runtime_error("Unterminated block comment at line " + std::to_string(line));
                    }
                    advance();
                }
                advance(); // '*'
                advance(); // '/'
                continue;
            }

            break;
        }
    }

    Token makeToken(TokenType t, const std::string &val = "") const
    {
        return Token{t, val, line, col};
    }

    Token identifier()
    {
        size_t start = pos;
        int startCol = col;
        while (std::isalnum(static_cast<unsigned char>(peek())) || peek() == '_')
            advance();
        std::string txt = src.substr(start, pos - start);
        auto it = keywords.find(txt);
        if (it != keywords.end())
        {
            return Token{it->second, txt, line, startCol};
        }
        return Token{TokenType::Identifier, txt, line, startCol};
    }

    Token number()
    {
        size_t start = pos;
        int startCol = col;
        // integer part
        while (std::isdigit(static_cast<unsigned char>(peek())))
            advance();

        // optional fractional part
        if (peek() == '.' && std::isdigit(static_cast<unsigned char>(peekNext())))
        {
            advance(); // consume '.'
            while (std::isdigit(static_cast<unsigned char>(peek())))
                advance();
        }

        std::string txt = src.substr(start, pos - start);
        return Token{TokenType::Number, txt, line, startCol};
    }

    Token stringLiteral()
    {
        // opening " already consumed by caller
        size_t start = pos;
        int startCol = col;
        std::string out;
        while (peek() != '"' && peek() != '\0')
        {
            if (peek() == '\\')
            {
                advance(); // consume '\'
                char esc = peek();
                if (esc == '\0')
                    break;
                // handle common escapes
                switch (esc)
                {
                case 'n':
                    out.push_back('\n');
                    break;
                case 'r':
                    out.push_back('\r');
                    break;
                case 't':
                    out.push_back('\t');
                    break;
                case '\\':
                    out.push_back('\\');
                    break;
                case '"':
                    out.push_back('"');
                    break;
                default:
                    // keep unknown escape as-is (e.g. \x)
                    out.push_back(esc);
                    break;
                }
                advance(); // consume escaped char
            }
            else
            {
                out.push_back(peek());
                advance();
            }
        }
        if (peek() != '"')
        {
            throw std::runtime_error("Unterminated string literal at line " + std::to_string(line));
        }
        advance(); // consume closing "
        return Token{TokenType::String, out, line, startCol};
    }

public:
    Lexer(const std::string &s) : src(s), pos(0), line(1), col(1) {}

    std::vector<Token> tokenize()
    {
        std::vector<Token> out;
        while (true)
        {
            skipWhitespaceAndComments();
            char c = peek();
            if (c == '\0')
            {
                out.push_back(makeToken(TokenType::Eof, ""));
                break;
            }

            // identifiers / keywords
            if (std::isalpha(static_cast<unsigned char>(c)) || c == '_')
            {
                // start identifier
                // don't advance here; identifier() expects to start at current pos
                out.push_back(identifier());
                continue;
            }

            // numbers
            if (std::isdigit(static_cast<unsigned char>(c)))
            {
                out.push_back(number());
                continue;
            }

            // punctuation, operators, strings
            switch (c)
            {
            case '(':
                advance();
                out.push_back(makeToken(TokenType::LParen, "("));
                break;
            case ')':
                advance();
                out.push_back(makeToken(TokenType::RParen, ")"));
                break;
            case '{':
                advance();
                out.push_back(makeToken(TokenType::LBrace, "{"));
                break;
            case '}':
                advance();
                out.push_back(makeToken(TokenType::RBrace, "}"));
                break;
            case '[':
                advance();
                out.push_back(makeToken(TokenType::LBracket, "["));
                break;
            case ']':
                advance();
                out.push_back(makeToken(TokenType::RBracket, "]"));
                break;
            case ':':
                advance();
                out.push_back(makeToken(TokenType::Colon, ":"));
                break;
            case ';':
                advance();
                out.push_back(makeToken(TokenType::Semicolon, ";"));
                break;
            case ',':
                advance();
                out.push_back(makeToken(TokenType::Comma, ","));
                break;
            case '+':
                advance();
                out.push_back(makeToken(TokenType::Plus, "+"));
                break;
            case '-':
                advance();
                out.push_back(makeToken(TokenType::Minus, "-"));
                break;
            case '*':
                advance();
                out.push_back(makeToken(TokenType::Star, "*"));
                break;
            case '/':
                // could be comment handled earlier; here it's division
                advance();
                out.push_back(makeToken(TokenType::Slash, "/"));
                break;
            case '%':
                advance();
                out.push_back(makeToken(TokenType::MOD, "%"));
                break;
            case '!':
            {
                advance();
                if (match('='))
                    out.push_back(makeToken(TokenType::NotEqual, "!="));
                else
                    out.push_back(makeToken(TokenType::Bang, "!"));
                break;
            }
            case '=':
            {
                advance();
                if (match('='))
                    out.push_back(makeToken(TokenType::Equal, "=="));
                else
                    out.push_back(makeToken(TokenType::Assign, "="));
                break;
            }
            case '&':
            {
                advance();
                if (match('&'))
                    out.push_back(makeToken(TokenType::AndAnd, "&&"));
                else
                    out.push_back(makeToken(TokenType::BitAnd, "&"));
                break;
            }
            case '|':
            {
                advance();
                if (match('|'))
                    out.push_back(makeToken(TokenType::OrOr, "||"));
                else
                    out.push_back(makeToken(TokenType::BitOr, "|"));
                break;
            }
            case '^':
                advance();
                out.push_back(makeToken(TokenType::BitXor, "^"));
                break;
            case '<':
            {
                advance();
                if (match('<'))
                    out.push_back(makeToken(TokenType::ShiftLeft, "<<"));
                else if (match('='))
                    out.push_back(makeToken(TokenType::LessEqual, "<="));
                else
                    out.push_back(makeToken(TokenType::Less, "<"));
                break;
            }
            case '>':
            {
                advance();
                if (match('>'))
                    out.push_back(makeToken(TokenType::ShiftRight, ">>"));
                else if (match('='))
                    out.push_back(makeToken(TokenType::GreaterEqual, ">="));
                else
                    out.push_back(makeToken(TokenType::Greater, ">"));
                break;
            }
            case '"':
            {
                advance(); // consume opening "
                out.push_back(stringLiteral());
                break;
            }
            case '?':
                advance();
                out.push_back(makeToken(TokenType::Question, "?"));
                break;
            default:
                throw std::runtime_error(std::string("Unexpected character in input: '") + c + "' at line " + std::to_string(line));
            }
        }
        return out;
    }
};

// Convenience helper used by the rest of the project
std::vector<Token> lexString(const std::string &s)
{
    Lexer lx(s);
    return lx.tokenize();
}
