#pragma once
#include <string>
#include <vector>
#include <memory>
#include <regex>

// Forward declaration
struct ASTNode;
struct Statement;
struct Expression;

// Smart pointer type aliases
using ASTNodePtr = std::shared_ptr<ASTNode>;
using StmtPtr = std::shared_ptr<Statement>;
using ExprPtr = std::shared_ptr<Expression>;

// ---------------------------
// Base AST Node
// ---------------------------
struct ASTNode {
    virtual ~ASTNode() = default;
};

// ---------------------------
// Expressions
// ---------------------------
struct Expression : ASTNode {};

// Literal expressions: int, float, bool, string
// Literal expressions: int, float, bool, string
struct LiteralExpr : Expression {
    std::string value;   // raw lexeme
    std::string type;    // "int", "float", "bool", "string"

    // Manual constructor (already exists)
    LiteralExpr(const std::string &val, const std::string &t) 
        : value(val), type(t) {}

    // New auto-detect constructor
    LiteralExpr(const std::string &val) : value(val)
    {
        if (std::regex_match(val, std::regex(R"(\d+)"))) {
            type = "int";
        } else if (std::regex_match(val, std::regex(R"(\d+\.\d+)"))) {
            type = "float";
        } else if (val == "true" || val == "false") {
            type = "bool";
        } else {
            type = "string";
        }
    }
};



// Identifier expression
struct IdentifierExpr : Expression {
    std::string name;
    IdentifierExpr(const std::string &n) : name(n) {}
};

// Unary expression: !, -
struct UnaryExpr : Expression {
    std::string op;   // operator
    ExprPtr rhs;
    UnaryExpr(const std::string &o, ExprPtr r) : op(o), rhs(r) {}
};

// Binary expression: +, -, *, /, %, &&, ||, <, <=, >, >=, ==, !=
struct BinaryExpr : Expression {
    ExprPtr lhs;
    std::string op;
    ExprPtr rhs;
    BinaryExpr(ExprPtr l, const std::string &o, ExprPtr r) : lhs(l), op(o), rhs(r) {}
};

// Function call: foo(a, b)
struct CallExpr : Expression {
    ExprPtr callee;                // IdentifierExpr usually
    std::vector<ExprPtr> args;
    CallExpr(ExprPtr c, std::vector<ExprPtr> a) : callee(c), args(std::move(a)) {}
};

// Array literal: [1,2,3]
struct ArrayExpr : Expression {
    std::vector<ExprPtr> elements;
    ArrayExpr(std::vector<ExprPtr> elems) : elements(std::move(elems)) {}
};



// Index expression: arr[0]
struct IndexExpr : Expression {
    ExprPtr array;
    ExprPtr index;
    IndexExpr(ExprPtr arr, ExprPtr idx) : array(arr), index(idx) {}
};

// ---------------------------
// Statements
// ---------------------------
struct Statement : ASTNode {};

// Variable declaration: let/var x = expr
struct VarDecl : Statement {
    bool isMutable; // true if var, false if let
    std::string name;
    std::string typeHint; // optional type
    ExprPtr initializer;
    VarDecl(bool mut, const std::string &n, ExprPtr init, const std::string &t = "")
    : isMutable(mut), name(n), typeHint(t), initializer(init) {}
};

// Expression statement: expr;
struct ExprStmt : Statement {
    ExprPtr expr;
    ExprStmt(ExprPtr e) : expr(e) {}
};

// Return statement
struct ReturnStmt : Statement {
    ExprPtr value; // can be nullptr for void
    ReturnStmt(ExprPtr v) : value(v) {}
};

// Block: { stmt1; stmt2; ... }
struct BlockStmt : Statement {
    std::vector<StmtPtr> statements;
    BlockStmt(std::vector<StmtPtr> stmts) : statements(std::move(stmts)) {}
};

// If statement: if (cond) { ... } else { ... }
struct IfStmt : Statement {
    ExprPtr condition;
    StmtPtr thenBranch;
    StmtPtr elseBranch; // optional
    IfStmt(ExprPtr cond, StmtPtr thenB, StmtPtr elseB = nullptr)
        : condition(cond), thenBranch(thenB), elseBranch(elseB) {}
};

struct IfExpr : public Expression {
    ExprPtr condition;
    ExprPtr thenBranch;
    ExprPtr elseBranch;

    IfExpr(ExprPtr cond, ExprPtr thenB, ExprPtr elseB)
        : condition(cond), thenBranch(thenB), elseBranch(elseB) {}
};


struct ArrayAssignExpr : Expression {
    ExprPtr array;
    ExprPtr index;
    ExprPtr value;

    ArrayAssignExpr(ExprPtr arr, ExprPtr idx, ExprPtr val)
        : array(arr), index(idx), value(val) {}
};


// While loop
struct WhileStmt : Statement {
    ExprPtr condition;
    StmtPtr body;
    WhileStmt(ExprPtr cond, StmtPtr b) : condition(cond), body(b) {}
};



// For loop: for x in array { ... }
struct ForStmt : Statement {
    std::string iterator;
    ExprPtr iterable;
    StmtPtr body;
    ForStmt(const std::string &it, ExprPtr iter, StmtPtr b)
        : iterator(it), iterable(iter), body(b) {}
};



// Times loop: 5 times { ... }
struct TimesStmt : Statement {
    ExprPtr count;
    StmtPtr body;
    TimesStmt(ExprPtr c, StmtPtr b) : count(c), body(b) {}
};

// Function declaration: fn foo(a:int, b:int): int { ... }
struct FuncDecl : Statement {
    std::string name;
    std::vector<std::pair<std::string, std::string>> params; // name + type
    std::string returnType; // optional
    StmtPtr body;           // usually BlockStmt
    FuncDecl(const std::string &n,
         std::vector<std::pair<std::string, std::string>> p,
         StmtPtr b,
         const std::string &ret = "")
    : name(n), params(std::move(p)), returnType(ret), body(b) {}
};


struct PrintStmt : Statement {
    ExprPtr expr;
    PrintStmt(const ExprPtr &e) : expr(e) {}
};

// Add this struct to your existing ast.h file
struct TypeInfo {
    std::string baseType;           // "int", "float", "bool", "string", "array"
    bool isNullable = false;        // true if type ends with '?'
    std::vector<int> dimensions;    // for multi-dimensional arrays [3][4] = {3, 4}
    std::shared_ptr<TypeInfo> elementType; // for arrays, type of elements
    
    TypeInfo() = default;
    TypeInfo(const std::string& base) : baseType(base) {}
    
    // Parse type string like "int[]?" or "float[3][4]"
    static std::shared_ptr<TypeInfo> parse(const std::string& typeStr) {
        auto type = std::make_shared<TypeInfo>();
        std::string workStr = typeStr;
        
        // Check for nullable
        if (!workStr.empty() && workStr.back() == '?') {
            type->isNullable = true;
            workStr.pop_back();
        }
        
        // Parse array dimensions
        size_t bracketPos = workStr.find('[');
        if (bracketPos != std::string::npos) {
            type->baseType = "array";
            std::string baseTypeStr = workStr.substr(0, bracketPos);
            type->elementType = std::make_shared<TypeInfo>(baseTypeStr);
            
            // Parse dimensions
            std::string dimStr = workStr.substr(bracketPos);
            std::regex dimRegex(R"(\[(\d*)\])");
            std::sregex_iterator iter(dimStr.begin(), dimStr.end(), dimRegex);
            std::sregex_iterator end;
            
            while (iter != end) {
                std::string dimMatch = (*iter)[1].str();
                if (dimMatch.empty()) {
                    type->dimensions.push_back(-1); // dynamic size
                } else {
                    type->dimensions.push_back(std::stoi(dimMatch));
                }
                ++iter;
            }
        } else {
            type->baseType = workStr;
        }
        
        return type;
    }
    
    std::string toString() const {
        std::string result = baseType;
        if (baseType == "array" && elementType) {
            result = elementType->baseType;
            for (int dim : dimensions) {
                result += "[";
                if (dim != -1) result += std::to_string(dim);
                result += "]";
            }
        }
        if (isNullable) result += "?";
        return result;
    }
    
    bool isArray() const { return baseType == "array"; }
    int getDimensionality() const { return dimensions.size(); }
    bool isDynamicSize(int dimIndex = 0) const { 
    return dimIndex < static_cast<int>(dimensions.size()) && dimensions[dimIndex] == -1; 
}
};