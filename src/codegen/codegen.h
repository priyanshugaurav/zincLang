// codegen.h
#pragma once
#include "../ast/ast.h"
#include <string>

class CodeGen {
public:
    std::string generate(const StmtPtr& program);

private:
    std::string genStmt(const StmtPtr& stmt);
    std::string genExpr(const ExprPtr& expr);
};
