#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "ast/ast.h"
#include "sema/environment.h"
#include "sema/typechecker.h"

#include "ir/ir.h"
#include <cstdlib> 
#include "codegen/codegen.h"


// ---------------------------
// Utilities
// ---------------------------
std::string readFile(const std::string &path)
{
    std::ifstream file(path);
    if (!file.is_open())
        throw std::runtime_error("Could not open file: " + path);
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

std::string tokenTypeToString(TokenType t)
{
    switch (t)
    {
    case TokenType::Let:
        return "Let";
    case TokenType::Var:
        return "Var";
    case TokenType::Fn:
        return "Fn";
    case TokenType::If:
        return "If";
    case TokenType::Else:
        return "Else";
    case TokenType::While:
        return "While";
    case TokenType::Return:
        return "Return";
    case TokenType::For:
        return "For";
    case TokenType::In:
        return "In";
    case TokenType::Times:
        return "Times";
    case TokenType::True:
        return "True";
    case TokenType::False:
        return "False";
    case TokenType::Then:
        return "Then";

    case TokenType::Identifier:
        return "Identifier";
    case TokenType::Number:
        return "Number";
    case TokenType::String:
        return "String";

    case TokenType::LParen:
        return "LParen";
    case TokenType::RParen:
        return "RParen";
    case TokenType::LBrace:
        return "LBrace";
    case TokenType::RBrace:
        return "RBrace";
    case TokenType::LBracket:
        return "LBracket";
    case TokenType::RBracket:
        return "RBracket";

    case TokenType::Colon:
        return "Colon";
    case TokenType::Semicolon:
        return "Semicolon";
    case TokenType::Comma:
        return "Comma";

    case TokenType::Plus:
        return "Plus";
    case TokenType::Minus:
        return "Minus";
    case TokenType::Star:
        return "Star";
    case TokenType::Slash:
        return "Slash";
    case TokenType::MOD:
        return "MOD";

    case TokenType::Assign:
        return "Assign";
    case TokenType::Equal:
        return "Equal";
    case TokenType::NotEqual:
        return "NotEqual";
    case TokenType::Bang:
        return "Bang";

    case TokenType::Less:
        return "Less";
    case TokenType::LessEqual:
        return "LessEqual";
    case TokenType::Greater:
        return "Greater";
    case TokenType::GreaterEqual:
        return "GreaterEqual";

    case TokenType::AndAnd:
        return "AndAnd";
    case TokenType::OrOr:
        return "OrOr";
    case TokenType::BitAnd:
        return "BitAnd";
    case TokenType::BitOr:
        return "BitOr";
    case TokenType::BitXor:
        return "BitXor";
    case TokenType::ShiftLeft:
        return "ShiftLeft";
    case TokenType::ShiftRight:
        return "ShiftRight";

    case TokenType::Eof:
        return "Eof";
    case TokenType::Null:
        return "Null";
    case TokenType::Question:
        return "Question";

    case TokenType::Unknown:
        return "Unknown";
    }

    return "???";
}

// ---------------------------
// Print AST
// ---------------------------
void printAST(const ExprPtr &expr, int indent = 0);
void printAST(const StmtPtr &stmt, int indent = 0);

std::string indentStr(int indent)
{
    return std::string(indent * 2, ' ');
}

void printAST(const ExprPtr &expr, int indent)
{
    if (!expr)
        return;

    auto ind = indentStr(indent);

    if (auto lit = std::dynamic_pointer_cast<LiteralExpr>(expr))
    {
        std::cout << ind << "Literal: " << lit->value
                  << " (type: " << lit->type << ")\n";
    }

    else if (auto id = std::dynamic_pointer_cast<IdentifierExpr>(expr))
    {
        std::cout << ind << "Identifier: " << id->name << "\n";
    }
    else if (auto bin = std::dynamic_pointer_cast<BinaryExpr>(expr))
    {
        std::cout << ind << "BinaryExpr: " << bin->op << "\n";
        printAST(bin->lhs, indent + 1);
        printAST(bin->rhs, indent + 1);
    }
    else if (auto un = std::dynamic_pointer_cast<UnaryExpr>(expr))
    {
        std::cout << ind << "UnaryExpr: " << un->op << "\n";
        printAST(un->rhs, indent + 1);
    }
    else if (auto call = std::dynamic_pointer_cast<CallExpr>(expr))
    {
        std::cout << ind << "CallExpr:\n";
        printAST(call->callee, indent + 1);
        for (auto &arg : call->args)
            printAST(arg, indent + 1);
    }
    else if (auto arr = std::dynamic_pointer_cast<ArrayExpr>(expr))
    {
        std::cout << ind << "ArrayExpr:\n";
        for (auto &el : arr->elements)
            printAST(el, indent + 1);
    }
    else if (auto idx = std::dynamic_pointer_cast<IndexExpr>(expr))
    {
        std::cout << ind << "IndexExpr:\n";
        printAST(idx->array, indent + 1);
        printAST(idx->index, indent + 1);
    }
    else if (auto ife = std::dynamic_pointer_cast<IfExpr>(expr))
    {
        std::cout << ind << "IfExpr:\n";
        std::cout << indentStr(indent + 1) << "Condition:\n";
        printAST(ife->condition, indent + 2);
        std::cout << indentStr(indent + 1) << "Then:\n";
        printAST(ife->thenBranch, indent + 2);
        if (ife->elseBranch)
        {
            std::cout << indentStr(indent + 1) << "Else:\n";
            printAST(ife->elseBranch, indent + 2);
        }
    }
    else if (auto arrAssign = std::dynamic_pointer_cast<ArrayAssignExpr>(expr))
    {
        std::cout << ind << "ArrayAssignExpr:\n";
        std::cout << ind << "  Array:\n";
        printAST(arrAssign->array, indent + 2);
        std::cout << ind << "  Index:\n";
        printAST(arrAssign->index, indent + 2);
        std::cout << ind << "  Value:\n";
        printAST(arrAssign->value, indent + 2);
    }
    else
    {
        std::cout << ind << "UnknownExpr\n";
    }
}

void printAST(const StmtPtr &stmt, int indent)
{
    auto ind = std::string(indent * 2, ' ');
    if (!stmt)
    {
        std::cout << ind << "nullptr statement\n";
        return;
    }

    if (auto v = std::dynamic_pointer_cast<VarDecl>(stmt))
    {
        std::cout << ind << "VarDecl: " << v->name << (v->isMutable ? " (var)" : " (let)") << "\n";
        printAST(v->initializer, indent + 1);
    }
    else if (auto f = std::dynamic_pointer_cast<FuncDecl>(stmt))
    {
        std::cout << ind << "FuncDecl: " << f->name << "\n";
        for (auto &[n, t] : f->params)
            std::cout << ind << "  Param: " << n << " : " << t << "\n";
        printAST(f->body, indent + 1);
    }
    else if (auto b = std::dynamic_pointer_cast<BlockStmt>(stmt))
    {
        std::cout << ind << "BlockStmt:\n";
        for (auto &s : b->statements)
            printAST(s, indent + 1);
        if (b->statements.empty())
            std::cout << ind << "  <empty>\n";
    }
    else if (auto e = std::dynamic_pointer_cast<ExprStmt>(stmt))
    {
        std::cout << ind << "ExprStmt:\n";
        printAST(e->expr, indent + 1);
    }
    else if (auto i = std::dynamic_pointer_cast<IfStmt>(stmt))
    {
        std::cout << ind << "IfStmt:\n";
        printAST(i->condition, indent + 1);
        printAST(i->thenBranch, indent + 1);
        if (i->elseBranch)
            printAST(i->elseBranch, indent + 1);
    }
    else if (auto w = std::dynamic_pointer_cast<WhileStmt>(stmt))
    {
        std::cout << ind << "WhileStmt:\n";
        printAST(w->condition, indent + 1);
        printAST(w->body, indent + 1);
    }
    else if (auto f = std::dynamic_pointer_cast<ForStmt>(stmt))
    {
        std::cout << ind << "ForStmt: " << f->iterator << "\n";
        printAST(f->iterable, indent + 1);
        printAST(f->body, indent + 1);
    }
    else if (auto t = std::dynamic_pointer_cast<TimesStmt>(stmt))
    {
        std::cout << ind << "TimesStmt:\n";
        printAST(t->count, indent + 1);
        printAST(t->body, indent + 1);
    }
    else if (auto r = std::dynamic_pointer_cast<ReturnStmt>(stmt))
    {
        std::cout << ind << "ReturnStmt:\n";
        printAST(r->value, indent + 1);
    }

    else
    {
        std::cout << ind << "UnknownStmt\n";
    }
}

// ---------------------------
// Main
// ---------------------------
int main(int argc, char *argv[])
{
    if (argc < 2)
    {
        std::cerr << "Usage: zinc <source.zinc>\n";
        return 1;
    }

    try
    {
        std::string source = readFile(argv[1]);
        auto tokens = lexString(source);

        // Print tokens
        for (auto &tok : tokens)
        {
            std::cout << tokenTypeToString(tok.type)
                      << " (" << tok.value << ")"
                      << " at line " << tok.line << ", col " << tok.col
                      << "\n";
        }

        // Parse
        Parser parser(tokens);
        // auto stmts = parser.parseProgram(); // vector<StmtPtr>
        StmtPtr ast = parser.parse(); // already returns a BlockStmt

        std::cout << "\n===== AST =====\n";
        printAST(ast);

        std::cout << "\n===== Type Checking =====\n";
        try
        {
            auto globalEnv = std::make_shared<Environment>();
            TypeChecker checker(globalEnv);
            checker.check(ast);
            std::cout << "✅ Type checking passed!\n";
        }

        catch (const std::exception &ex)
        {
            std::cerr << "Type Error: " << ex.what() << "\n";
            return 1;
        }

        // std::cout << "\n===== LLVM IR =====\n";

        // // Create a Codegen instance
        // Codegen cg("ZincModule");

        // // Define a main function
        // llvm::FunctionType *mainTy =
        //     llvm::FunctionType::get(llvm::Type::getInt32Ty(cg.context), false);
        // llvm::Function *mainFunc =
        //     llvm::Function::Create(mainTy, llvm::Function::ExternalLinkage, "main", cg.getModule());

        // llvm::BasicBlock *entry = llvm::BasicBlock::Create(cg.context, "entry", mainFunc);
        // cg.builder.SetInsertPoint(entry);

        // // Lower the AST into LLVM IR
        // cg.codegenStmt(ast);

        // // Ensure function ends with return
        // if (!entry->getTerminator())
        // {
        //     cg.builder.CreateRet(llvm::ConstantInt::get(cg.context, llvm::APInt(32, 0)));
        // }

        // // Verify function
        // llvm::verifyFunction(*mainFunc);

        // // Print IR to console
        // cg.getModule()->print(llvm::outs(), nullptr);

        // // Also save to file
        // std::error_code EC;
        // llvm::raw_fd_ostream dest("output.ll", EC);
        // cg.getModule()->print(dest, nullptr);
        // std::cout << "\n✅ LLVM IR written to output.ll\n";
        std::cout << "\n===== NASM Codegen =====\n";
        nasm::Codegen cg("output.asm");
        cg.generate(ast);
        std::cout << "✅ Assembly written to output.asm\n";
        // std::system("nasm -f elf64 output.asm");
        // std::system("ld output.o");
        // std::system("./a.out");
        

    }
    catch (const std::exception &ex)
    {
        std::cerr << "Error: " << ex.what() << "\n";
        return 1;
    }

    return 0;
}
