#include <iostream>
#include <fstream>
#include "lexer/lexer.h"
#include "parser/parser.h"

int main() {
    std::ifstream file("../examples/operators.zinc");
    std::string src((std::istreambuf_iterator<char>(file)),
                     std::istreambuf_iterator<char>());

    Lexer lexer(src);
    std::vector<Token> tokens;
    Token tok = lexer.nextToken();
    while (tok.type != TokenType::END_OF_FILE) {
        tokens.push_back(tok);
        tok = lexer.nextToken();
    }
    tokens.push_back(tok); // push EOF token

    Parser parser(tokens);
    try {
        auto program = parser.parse();
        std::cout << "Parsed successfully: " << program.size() << " top-level statements\n\n";
        Parser::printAST(program);
    } catch (const std::exception &e) {
        std::cerr << "Parser error: " << e.what() << std::endl;
    }
    return 0;
}
