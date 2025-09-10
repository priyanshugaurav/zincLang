#pragma once
#include "../ast/ast.h"
#include "environment.h"
#include <memory>
#include <string>
#include <optional>
#include <stdexcept>

class TypeChecker {
public:
    // Constructor takes a reference to the environment
    explicit TypeChecker(std::shared_ptr<Environment> env);

    // Entry point: check a top-level block or statement
    void check(const StmtPtr &program);

private:
    // current lexical environment
    std::shared_ptr<Environment> env;
    std::shared_ptr<Environment> currentScope;

    // current function return type when traversing a function body
    // empty string means 'void' / unspecified
    std::string currentReturnType;

    // Helpers
    void checkStmt(const StmtPtr &stmt);
    void checkBlock(const std::vector<StmtPtr> &stmts);

    // expression returns a type string like "int", "float", "bool", ...
    std::string inferExpr(const ExprPtr &expr);

    // utility
    bool isNumericType(const std::string &t) const;
    bool isIntType(const std::string &t) const;
    bool isFloatType(const std::string &t) const;
    bool isBoolType(const std::string &t) const;
    bool isStringType(const std::string &t) const;
    bool isArrayType(const std::string &t) const;
    std::string arrayElementType(const std::string &arrayType) const;
};
