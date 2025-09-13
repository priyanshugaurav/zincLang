#pragma once
#include "../ast/ast.h"
#include "environment.h"
#include <memory>
#include <string>
#include <optional>
#include <stdexcept>

class TypeChecker {
public:
    TypeChecker(std::shared_ptr<Environment> env);

    // --- Utility type checks ---
    bool isNumericType(const std::string &t) const;
    bool isIntType(const std::string &t) const;
    bool isFloatType(const std::string &t) const;
    bool isBoolType(const std::string &t) const;
    bool isStringType(const std::string &t) const;
    bool isArrayType(const std::string &t) const;
    std::string arrayElementType(const std::string &arrayType) const;
    bool isAnyType(const std::string &t) const;

    // 🔥 Add this declaration
    bool isArrayCompatible(const std::string &declared, const std::string &inferred) const;

    // ---------------------------
    void check(const StmtPtr &program);
    void checkStmt(const StmtPtr &stmt);
    void checkBlock(const std::vector<StmtPtr> &stmts);
    std::string inferExpr(const ExprPtr &expr);

private:
    std::shared_ptr<Environment> env;
    std::shared_ptr<Environment> currentScope;
    std::string currentReturnType;
};
