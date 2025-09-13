#include "typechecker.h"
#include "../runtime/builtins.h"
#include <iostream>
#include <sstream>
#include <regex>

TypeChecker::TypeChecker(std::shared_ptr<Environment> env)
    : env(std::move(env)), currentReturnType("")
{
    // register builtins in env, not in a separate currentScope
    registerBuiltins(*this->env);

    // currentScope can be initialized as child of env if needed
    currentScope = std::make_shared<Environment>(this->env);
}

// Utility type checks
bool TypeChecker::isNumericType(const std::string &t) const
{
    return t == "int" || t == "float";
}
bool TypeChecker::isIntType(const std::string &t) const { return t == "int"; }
bool TypeChecker::isFloatType(const std::string &t) const { return t == "float"; }
bool TypeChecker::isBoolType(const std::string &t) const { return t == "bool"; }
bool TypeChecker::isStringType(const std::string &t) const { return t == "string"; }
bool TypeChecker::isArrayType(const std::string &t) const
{
    return t.rfind("array<", 0) == 0 && t.back() == '>';
}
std::string TypeChecker::arrayElementType(const std::string &arrayType) const
{
    // expected format: array<elem>
    if (!isArrayType(arrayType))
        return "";
    return arrayType.substr(6, arrayType.size() - 7);
}

bool TypeChecker::isAnyType(const std::string &t) const 
{ 
    return t == "any"; 
}

bool TypeChecker::isArrayCompatible(const std::string &declared, const std::string &inferred) const {
    if (declared == inferred)
        return true;

    if (isArrayType(declared) && isArrayType(inferred)) {
        std::string dElem = arrayElementType(declared);
        std::string iElem = arrayElementType(inferred);

        // Allow widening: inferred elem is T, declared is T?
        if (!dElem.empty() && dElem.back() == '?' && dElem.substr(0, dElem.size() - 1) == iElem)
            return true;

        // recursive check (for nested arrays etc.)
        return isArrayCompatible(dElem, iElem);
    }

    return false;
}



// ---------------------------
// Top-level check
// ---------------------------
void TypeChecker::check(const StmtPtr &program)
{
    // Expect a BlockStmt as returned by parser.parse()
    if (!program)
        return;
    if (auto b = std::dynamic_pointer_cast<BlockStmt>(program))
    {
        checkBlock(b->statements);
        return;
    }
    // single stmt program
    checkStmt(program);
}

void TypeChecker::checkBlock(const std::vector<StmtPtr> &stmts)
{
    // Create child environment for this block
    auto oldEnv = env;
    env = std::make_shared<Environment>(oldEnv);

    for (auto &s : stmts)
    {
        checkStmt(s);
    }

    env = oldEnv;
}



// ---------------------------
// Statements
// ---------------------------
void TypeChecker::checkStmt(const StmtPtr &stmt)
{
    if (!stmt)
        return;

    // ---------------------------
    // VarDecl
    if (auto v = std::dynamic_pointer_cast<VarDecl>(stmt))
    {
        bool nullable = false;
        std::string baseType = v->typeHint;

        // check for nullable type
        if (!v->typeHint.empty() && v->typeHint.back() == '?')
        {
            nullable = true;
            baseType.pop_back(); // remove '?'
        }
        if (!baseType.empty() && !nullable && !v->initializer)
        {
            throw std::runtime_error("Variable '" + v->name + "' of non-nullable type '" + baseType + "' must be initialized at declaration");
        }

        // Infer type for untyped vars
        bool isDynamic = false;
        if (baseType.empty())
        {
            isDynamic = true; // no type hint => dynamic
            if (v->initializer)
            {
                baseType = inferExpr(v->initializer);
            }
            else
            {
                baseType = "dynamic"; // placeholder type until assigned
            }
        }

        // initializer exists, check type match for explicitly typed
        if (v->initializer)
        {
            std::string initType = inferExpr(v->initializer);

            if (initType == "null")
            {
                if (!nullable)
                {
                    throw std::runtime_error("Cannot assign null to non-nullable variable '" + v->name + "'");
                }
            }
            else if (!isDynamic && !isArrayCompatible(baseType, initType))
            {
                throw std::runtime_error("Type mismatch for variable '" + v->name +
                                         "': expected " + baseType + ", got " + initType);
            }

            // If dynamic, update type to initializer type
            if (isDynamic)
                baseType = initType;
        }

        // define variable in environment
        if (!env->define(v->name, baseType, v->isMutable, -1, isDynamic, nullable))
        {
            throw std::runtime_error("Symbol already defined in this scope: " + v->name);
        }

        return;
    }

    // ExprStmt
    if (auto e = std::dynamic_pointer_cast<ExprStmt>(stmt))
    {
        // expression must be well-typed
        inferExpr(e->expr);
        return;
    }

    // ReturnStmt
// ---------------------------
// Statements
// ---------------------------

    // ReturnStmt
    if (auto r = std::dynamic_pointer_cast<ReturnStmt>(stmt))
    {
        if (r->value)
        {
            std::string t = inferExpr(r->value);

            // If in inference mode (function had no declared return type),
            // allow the first return to set the inferred type.
            if (currentReturnType == "__INFER_RET__")
            {
                // set inferred return type
                currentReturnType = t;
                return;
            }

            // If currentReturnType empty -> function declared void / no return allowed with value
            if (currentReturnType.empty())
            {
                throw std::runtime_error("Return with value in a function declared void / no return type");
            }

            if (t != currentReturnType)
            {
                // allow int -> float widening
                if (!(isIntType(t) && isFloatType(currentReturnType)))
                    throw std::runtime_error("Return type mismatch: expected " + currentReturnType + ", got " + t);
            }
        }
        else
        {
            // return without value
            if (currentReturnType.empty())
            {
                // function declared void -> okay
                return;
            }

            if (currentReturnType == "__INFER_RET__")
            {
                // no previous typed return seen, mark inferred as void
                currentReturnType = "void";
                return;
            }

            if (currentReturnType != "void")
            {
                throw std::runtime_error("Return without value in function that expects type " + currentReturnType);
            }
        }
        return;
    }

    // BlockStmt
    if (auto b = std::dynamic_pointer_cast<BlockStmt>(stmt))
    {
        checkBlock(b->statements);
        return;
    }

    // IfStmt
    if (auto ifs = std::dynamic_pointer_cast<IfStmt>(stmt))
    {
        std::string condType = inferExpr(ifs->condition);
        if (!isBoolType(condType))
        {
            throw std::runtime_error("If condition must be bool, got " + condType);
        }
        checkStmt(ifs->thenBranch);
        if (ifs->elseBranch)
            checkStmt(ifs->elseBranch);
        return;
    }

    // WhileStmt
    if (auto w = std::dynamic_pointer_cast<WhileStmt>(stmt))
    {
        std::string condType = inferExpr(w->condition);
        if (!isBoolType(condType))
        {
            throw std::runtime_error("While condition must be bool, got " + condType);
        }
        checkStmt(w->body);
        return;
    }

    // ForStmt: for iterator in iterable { ... }
    if (auto f = std::dynamic_pointer_cast<ForStmt>(stmt))
    {
        std::string iterType = inferExpr(f->iterable);
        if (!isArrayType(iterType))
        {
            throw std::runtime_error("For loop iterable must be array, got " + iterType);
        }
        std::string elemType = arrayElementType(iterType);

        // new child env with iterator declared as let
        auto oldEnv = env;
        env = std::make_shared<Environment>(oldEnv);
        if (!env->define(f->iterator, elemType, false))
        {
            throw std::runtime_error("Iterator name already used in this scope: " + f->iterator);
        }

        checkStmt(f->body);

        env = oldEnv;
        return;
    }

    // TimesStmt: count must be int
    if (auto t = std::dynamic_pointer_cast<TimesStmt>(stmt))
    {
        std::string countType = inferExpr(t->count);
        if (!isIntType(countType))
        {
            throw std::runtime_error("times count must be int, got " + countType);
        }
        checkStmt(t->body);
        return;
    }

    // FuncDecl
    // FuncDecl
    if (auto fn = std::dynamic_pointer_cast<FuncDecl>(stmt))
    {
        // build function type string for storage e.g. fn(int,int)->int
        std::ostringstream sig;
        sig << "fn(";
        for (size_t i = 0; i < fn->params.size(); ++i)
        {
            if (i)
                sig << ",";
            // params[i].second may be empty -> treat as "any"
            std::string ptype = fn->params[i].second.empty() ? "any" : fn->params[i].second;
            sig << ptype;
        }
        sig << ")->";
        // If return type not declared, use "any" in the signature so callers accept it.
        // The body checking will enter an inference mode to validate returns inside the function body.
        std::string retType = fn->returnType.empty() ? "any" : fn->returnType;
        sig << (retType.empty() ? "void" : retType);

        // define function symbol in current env (so functions are first-class referencable)
        if (!env->define(fn->name, sig.str(), false))
        {
            throw std::runtime_error("Function already defined: " + fn->name);
        }

        // Check body with a new env: params in scope
        auto oldEnv = env;
        env = std::make_shared<Environment>(oldEnv);

        // define params (use "any" for missing param types)
        for (auto &p : fn->params)
        {
            const std::string &pname = p.first;
            const std::string ptype = p.second.empty() ? "any" : p.second;
            if (!env->define(pname, ptype, false))
            {
                throw std::runtime_error("Parameter name already used in function '" + fn->name + "': " + pname);
            }
        }

        // set current return type
        std::string oldReturn = currentReturnType;

        // if no declared return type, enable inference mode
        if (fn->returnType.empty())
            currentReturnType = "__INFER_RET__";
        else
            currentReturnType = fn->returnType;

        // body should be a BlockStmt normally
        if (auto bodyBlock = std::dynamic_pointer_cast<BlockStmt>(fn->body))
        {
            checkBlock(bodyBlock->statements);
        }
        else
        {
            // single-statement function bodies (not typical) - check directly
            checkStmt(fn->body);
        }

        // If function had no declared return type and inference left it as "__INFER_RET__",
        // it means no return statements were present -> treat as void (no value returned).
        if (fn->returnType.empty() && currentReturnType == "__INFER_RET__")
        {
            currentReturnType = "void";
        }

        // restore
        currentReturnType = oldReturn;
        env = oldEnv;
        return;
    }

    // Unknown statement
    throw std::runtime_error("Unhandled statement in typechecker");
}

// ---------------------------
// Expressions
// ---------------------------
std::string TypeChecker::inferExpr(const ExprPtr &expr)
{
    if (!expr)
        return "void";

    // Literals
    // Literals
    if (auto lit = std::dynamic_pointer_cast<LiteralExpr>(expr))
    {
        if (lit->type.empty())
            throw std::runtime_error("Literal has unknown type: " + lit->value);
        return lit->type; // <- use stored type
    }

    // Identifier
    if (auto id = std::dynamic_pointer_cast<IdentifierExpr>(expr))
    {
        auto symOpt = env->lookup(id->name);
        if (!symOpt.has_value())
        {
            throw std::runtime_error("Variable not defined: " + id->name);
        }
        return symOpt->type;
    }

    // Unary
    if (auto un = std::dynamic_pointer_cast<UnaryExpr>(expr))
    {
        std::string rt = inferExpr(un->rhs);
        if (un->op == "!")
        {
            if (!isBoolType(rt))
                throw std::runtime_error("Unary ! requires bool operand");
            return "bool";
        }
        if (un->op == "-")
        {
            if (!isNumericType(rt))
                throw std::runtime_error("Unary - requires numeric operand");
            return rt;
        }
        throw std::runtime_error("Unknown unary operator: " + un->op);
    }

    // Binary
if (auto bin = std::dynamic_pointer_cast<BinaryExpr>(expr)) {
    const std::string &op = bin->op;

    // ---------------- ASSIGNMENT (=) ----------------
    if (op == "=") {
        auto lhsId = std::dynamic_pointer_cast<IdentifierExpr>(bin->lhs);
        if (!lhsId)
            throw std::runtime_error("Invalid assignment target");

        std::string rhsType = inferExpr(bin->rhs);
        auto symOpt = env->lookup(lhsId->name);
        if (!symOpt.has_value())
            throw std::runtime_error("Assignment to undefined variable: " + lhsId->name);

        if (!symOpt->isMutable)
            throw std::runtime_error("Cannot assign to immutable variable (let): " + lhsId->name);

        // Null check
        if (rhsType == "null") {
            if (!symOpt->isNullable)
                throw std::runtime_error("Cannot assign null to non-nullable variable '" + lhsId->name + "'");
            return symOpt->type; // keep declared type
        }

        // Dynamic vars → update type
        if (symOpt->isDynamic) {
            symOpt->type = rhsType;
            return symOpt->type;
        }

        // Type check
        if (rhsType != symOpt->type) {
    if (!(isIntType(rhsType) && isFloatType(symOpt->type)) &&
        !isArrayCompatible(symOpt->type, rhsType)) {
        throw std::runtime_error("Assignment type mismatch for '" + lhsId->name +
                                 "': expected " + symOpt->type + ", got " + rhsType);
    }
}


        return symOpt->type;
    }

    // ---------------- ARITHMETIC ----------------
    if (op == "+" || op == "-" || op == "*" || op == "/" || op == "%") {
        std::string L = inferExpr(bin->lhs);
        std::string R = inferExpr(bin->rhs);

        if (op == "+" && isStringType(L) && isStringType(R))
            return "string";

        if (!isNumericType(L) || !isNumericType(R))
            throw std::runtime_error("Arithmetic requires numeric operands");

        return (isFloatType(L) || isFloatType(R)) ? "float" : "int";
    }

    // ---------------- COMPARISONS ----------------
    if (op == "==" || op == "!=") {
        std::string L = inferExpr(bin->lhs);
        std::string R = inferExpr(bin->rhs);

        if (L != R && !(isNumericType(L) && isNumericType(R)))
            throw std::runtime_error("Comparison requires same or compatible types, got " + L + " and " + R);

        return "bool";
    }

    if (op == "<" || op == "<=" || op == ">" || op == ">=") {
        std::string L = inferExpr(bin->lhs);
        std::string R = inferExpr(bin->rhs);

        if (!isNumericType(L) || !isNumericType(R))
            throw std::runtime_error("Comparison requires numeric types, got " + L + " and " + R);

        return "bool";
    }

    // ---------------- LOGICAL ----------------
    if (op == "&&" || op == "||") {
        std::string L = inferExpr(bin->lhs);
        std::string R = inferExpr(bin->rhs);

        if (!isBoolType(L) || !isBoolType(R))
            throw std::runtime_error("Logical operators require bool operands");

        return "bool";
    }

    throw std::runtime_error("Unknown or unsupported binary operator: " + op);
}

// ---------------- UNARY ----------------
if (auto u = std::dynamic_pointer_cast<UnaryExpr>(expr)) {
    std::string operandType = inferExpr(u->rhs);

    if (u->op == "-") {
        if (!isNumericType(operandType))
            throw std::runtime_error("Unary - requires numeric type, got " + operandType);
        return operandType;
    }

    if (u->op == "!") {
        if (operandType != "bool")
            throw std::runtime_error("Unary ! requires bool type, got " + operandType);
        return "bool";
    }

    throw std::runtime_error("Unsupported unary operator: " + u->op);
}

    // CallExpr
    // CallExpr
    if (auto call = std::dynamic_pointer_cast<CallExpr>(expr))
    {
        // callee usually IdentifierExpr
        std::string calleeType = inferExpr(call->callee); // will throw if not defined
        // calleeType expected like "fn(int,int)->int" or fn(...)->any
        if (calleeType.rfind("fn(", 0) != 0)
        {
            throw std::runtime_error("Attempting to call non-function type: " + calleeType);
        }
        // parse signature
        size_t p1 = calleeType.find('(');
        size_t p2 = calleeType.find(")->");
        if (p1 == std::string::npos || p2 == std::string::npos)
            throw std::runtime_error("Malformed function type: " + calleeType);
        std::string paramsStr = calleeType.substr(p1 + 1, p2 - (p1 + 1));
        std::string retStr = calleeType.substr(p2 + 3);
        // split params
        std::vector<std::string> paramTypes;
        if (!paramsStr.empty())
        {
            std::istringstream ss(paramsStr);
            std::string part;
            while (std::getline(ss, part, ','))
                paramTypes.push_back(part);
        }
        if (paramTypes.size() != call->args.size())
        {
            throw std::runtime_error("Function call arity mismatch: expected " + std::to_string(paramTypes.size()) + ", got " + std::to_string(call->args.size()));
        }
        for (size_t i = 0; i < call->args.size(); ++i)
        {
            std::string argType = inferExpr(call->args[i]);
            std::string paramType = paramTypes[i];

            if (paramType == "any")
                continue; // accept any type

            // allow int -> float widening
            if (argType != paramType)
            {
                if (!(isIntType(argType) && isFloatType(paramType)))
                    throw std::runtime_error("Function call argument " + std::to_string(i) +
                                             " type mismatch: expected " + paramType + ", got " + argType);
            }
        }

        // retStr "void" means empty
        if (retStr == "void")
            return "";
        return retStr;
    }


    // IndexExpr: arr[idx]
    if (auto idx = std::dynamic_pointer_cast<IndexExpr>(expr))
    {
        std::string arrType = inferExpr(idx->array);
        if (!isArrayType(arrType))
            throw std::runtime_error("Indexing requires array type, got " + arrType);

        std::string idxType = inferExpr(idx->index);
        if (!isIntType(idxType))
            throw std::runtime_error("Array index must be int, got " + idxType);

        // Compile-time bounds check
        auto symOpt = std::dynamic_pointer_cast<IdentifierExpr>(idx->array)
                          ? env->lookup(std::dynamic_pointer_cast<IdentifierExpr>(idx->array)->name)
                          : std::nullopt;

        int arrSize = -1;
        if (symOpt.has_value())
            arrSize = symOpt->arraySize;

        if (arrSize != -1)
        { // size known at compile time
            if (auto idxLit = std::dynamic_pointer_cast<LiteralExpr>(idx->index))
            {
                int indexValue = std::stoi(idxLit->value);
                if (indexValue < 0 || indexValue >= arrSize)
                {
                    throw std::runtime_error(
                        "Array index out of bounds at compile time: " +
                        std::to_string(indexValue) + " (array size " + std::to_string(arrSize) + ")");
                }
            }
        }

        return arrayElementType(arrType);
    }

    // IfExpr (expression form)
    if (auto ie = std::dynamic_pointer_cast<IfExpr>(expr))
    {
        std::string condType = inferExpr(ie->condition);
        if (!isBoolType(condType))
            throw std::runtime_error("If expression condition must be bool");
        std::string thenT = inferExpr(ie->thenBranch);
        std::string elseT = ie->elseBranch ? inferExpr(ie->elseBranch) : std::string();
        if (elseT.empty())
            throw std::runtime_error("If expression must have else branch");
        if (thenT != elseT)
        {
            if (!(isIntType(thenT) && isFloatType(elseT)) && !(isFloatType(thenT) && isIntType(elseT)))
            {
                throw std::runtime_error("If expression branches must have same type, got " + thenT + " and " + elseT);
            }
            if (isFloatType(thenT) || isFloatType(elseT))
                return "float";
        }
        return thenT;
    }




    // ArrayAssignExpr (array[index] = value) - in your AST it's an Expr node
// ArrayExpr

if (auto aa = std::dynamic_pointer_cast<ArrayAssignExpr>(expr))
{
    std::string arrType = inferExpr(aa->array);
    if (!isArrayType(arrType))
        throw std::runtime_error("Array assignment target is not array");

    // CHECK MUTABILITY
    if (auto arrId = std::dynamic_pointer_cast<IdentifierExpr>(aa->array))
    {
        auto symOpt = env->lookup(arrId->name);
        if (!symOpt.has_value())
            throw std::runtime_error("Array variable not found: " + arrId->name);
            
        if (!symOpt->isMutable)
            throw std::runtime_error("Cannot assign to immutable array (let): " + arrId->name);
    }

    std::string idxType = inferExpr(aa->index);
    if (!isIntType(idxType))
        throw std::runtime_error("Array index must be int");

    // COMPILE-TIME BOUNDS CHECK
    if (auto arrLit = std::dynamic_pointer_cast<ArrayExpr>(aa->array))
    {
        if (auto idxLit = std::dynamic_pointer_cast<LiteralExpr>(aa->index))
        {
            int indexValue = std::stoi(idxLit->value);
            int arrSize = static_cast<int>(arrLit->elements.size());
            if (indexValue < 0 || indexValue >= arrSize)
            {
                throw std::runtime_error(
                    "Array index out of bounds at compile time: " +
                    std::to_string(indexValue) + " (array size " + std::to_string(arrSize) + ")");
            }
        }
    }

    // Type checking for assignment
    std::string elemType = arrayElementType(arrType);
    std::string valType = inferExpr(aa->value);
    
    // If array element type is "any", allow any assignment
    if (isAnyType(elemType))
    {
        return valType;  // Return the actual assigned type
    }
    
    // Otherwise, enforce type compatibility
    if (valType != elemType)
    {
        if (!(isIntType(valType) && isFloatType(elemType)))
            throw std::runtime_error("Array assignment type mismatch: expected " + elemType + ", got " + valType);
    }

    return elemType;
}

// ArrayExpr - Updated to handle dynamic/heterogeneous arrays
// ArrayExpr - Updated to handle dynamic/heterogeneous arrays
if (auto arr = std::dynamic_pointer_cast<ArrayExpr>(expr)) {
    std::string unifiedType;
    bool sawNull = false;

    for (auto &el : arr->elements) {
        std::string elemType = inferExpr(el);

        if (elemType == "null") {
            sawNull = true;
            continue;
        }

        if (unifiedType.empty()) {
            unifiedType = elemType;
        } else if (unifiedType != elemType) {
            // If conflict, fallback to any
            unifiedType = "any";
        }
    }

    if (unifiedType.empty()) {
        unifiedType = "any";
    }

    if (sawNull) {
        // if already nullable, don’t double-add ?
        if (unifiedType != "any" && unifiedType.back() != '?') {
            unifiedType += "?";
        }
    }

    return "array<" + unifiedType + ">";
}

throw std::runtime_error("Unhandled expression type in typechecker");
}
