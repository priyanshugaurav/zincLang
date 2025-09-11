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
    // ---------------------------
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

        // if no initializer
        if (!v->initializer)
        {
            if (baseType.empty())
            {
                throw std::runtime_error("Variable '" + v->name +
                                         "' must have either initializer or type hint");
            }

            // non-nullable must have initializer
            if (!nullable)
            {
                throw std::runtime_error("Variable '" + v->name +
                                         "' of type '" + baseType +
                                         "' must be initialized (non-nullable)");
            }

            // nullable vars can be uninitialized (defaults to null)
            if (!env->define(v->name, baseType, v->isMutable, -1, false, true))
            {
                throw std::runtime_error("Symbol already defined in this scope: " + v->name);
            }
            return; // done
        }

        // initializer exists
        std::string initType = inferExpr(v->initializer);

        // allow null only for nullable types
        if (initType == "null")
        {
            if (!nullable)
            {
                throw std::runtime_error("Cannot assign null to non-nullable variable '" + v->name + "'");
            }
        }
        else
        {
            if (initType != baseType)
            {
                throw std::runtime_error("Type mismatch for variable '" + v->name +
                                         "': expected " + baseType + ", got " + initType);
            }
        }

        // define variable in environment
        bool isDyn = v->isMutable; // mutable = dynamic
        if (!env->define(v->name, baseType, v->isMutable, -1, isDyn, nullable))
        {
            throw std::runtime_error("Symbol already defined in this scope: " + v->name);
        }

        return; // done
    }

    // ExprStmt
    if (auto e = std::dynamic_pointer_cast<ExprStmt>(stmt))
    {
        // expression must be well-typed
        inferExpr(e->expr);
        return;
    }

    // ReturnStmt
    if (auto r = std::dynamic_pointer_cast<ReturnStmt>(stmt))
    {
        if (r->value)
        {
            std::string t = inferExpr(r->value);
            if (currentReturnType.empty())
            {
                throw std::runtime_error("Return with value in a function declared void / no return type");
            }
            if (t != currentReturnType)
            {
                throw std::runtime_error("Return type mismatch: expected " + currentReturnType + ", got " + t);
            }
        }
        else
        {
            if (!currentReturnType.empty())
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
    if (auto fn = std::dynamic_pointer_cast<FuncDecl>(stmt))
    {
        // build function type string for storage e.g. fn(int,int)->int
        std::ostringstream sig;
        sig << "fn(";
        for (size_t i = 0; i < fn->params.size(); ++i)
        {
            if (i)
                sig << ",";
            sig << fn->params[i].second;
        }
        sig << ")->";
        std::string retType = fn->returnType.empty() ? "" : fn->returnType;
        sig << (retType.empty() ? "void" : retType);

        // define function symbol in current env (so functions are first-class referencable)
        if (!env->define(fn->name, sig.str(), false))
        {
            throw std::runtime_error("Function already defined: " + fn->name);
        }

        // Check body with a new env: params in scope
        auto oldEnv = env;
        env = std::make_shared<Environment>(oldEnv);

        // define params
        for (auto &p : fn->params)
        {
            const std::string &pname = p.first;
            const std::string &ptype = p.second;
            if (!env->define(pname, ptype, false))
            {
                throw std::runtime_error("Parameter name already used in function '" + fn->name + "': " + pname);
            }
        }

        // set current return type
        std::string oldReturn = currentReturnType;
        currentReturnType = retType;

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
    if (auto bin = std::dynamic_pointer_cast<BinaryExpr>(expr))
    {
        // assignment operator handled here: lhs must be identifier
        if (bin->op == "=")
        {
            std::string valType = inferExpr(bin->rhs);

            if (auto lhsId = std::dynamic_pointer_cast<IdentifierExpr>(bin->lhs))
            {
                auto symOpt = env->lookup(lhsId->name);
                if (!symOpt.has_value())
                    throw std::runtime_error("Assignment to undefined variable: " + lhsId->name);

                // immutability check
                if (!symOpt->isMutable)
                    throw std::runtime_error("Cannot assign to immutable variable (let): " + lhsId->name);

                // ✅ handle nullable
                if (valType == "null")
                {
                    if (!symOpt->isNullable)
                        throw std::runtime_error("Cannot assign null to non-nullable variable '" + lhsId->name + "'");
                    return symOpt->type; // null is allowed for nullable
                }

                // type match or int->float widening
                if (valType != symOpt->type)
                {
                    if (!symOpt->isDynamic) // consider dynamic
                    {
                        if (!(isIntType(valType) && isFloatType(symOpt->type)))
                            throw std::runtime_error("Assignment type mismatch for '" + lhsId->name +
                                                     "': expected " + symOpt->type + ", got " + valType);
                    }
                    else
                    {
                        symOpt->type = valType; // dynamic type update
                    }
                }

                return symOpt->type;
            }

            throw std::runtime_error("Invalid assignment target");
        }

        // arithmetic ops
        const std::string &op = bin->op;
        if (op == "+" || op == "-" || op == "*" || op == "/" || op == "%")
        {
            std::string L = inferExpr(bin->lhs);
            std::string R = inferExpr(bin->rhs);

            // string concatenation with +
            if (op == "+" && isStringType(L) && isStringType(R))
                return "string";

            if (!isNumericType(L) || !isNumericType(R))
            {
                throw std::runtime_error("Arithmetic operator requires numeric operands");
            }
            // if either is float -> float, else int
            if (isFloatType(L) || isFloatType(R))
                return "float";
            return "int";
        }

        // comparisons -> bool
        if (op == "==" || op == "!=" || op == "<" || op == "<=" || op == ">" || op == ">=")
        {
            std::string L = inferExpr(bin->lhs);
            std::string R = inferExpr(bin->rhs);
            // allow comparisons on numeric and strings and bools of same type
            if (L != R)
            {
                // allow numeric comparsion between int and float
                if (isNumericType(L) && isNumericType(R))
                    return "bool";
                throw std::runtime_error("Comparison operands must be of same or compatible type");
            }
            return "bool";
        }

        // logical ops
        if (op == "&&" || op == "||")
        {
            std::string L = inferExpr(bin->lhs);
            std::string R = inferExpr(bin->rhs);
            if (!isBoolType(L) || !isBoolType(R))
                throw std::runtime_error("Logical operators require bool operands");
            return "bool";
        }

        // bitwise/logical not supported in detail here - fallthrough error
        throw std::runtime_error("Unknown or unsupported binary operator: " + op);
    }

    // CallExpr
    if (auto call = std::dynamic_pointer_cast<CallExpr>(expr))
    {
        // callee usually IdentifierExpr
        std::string calleeType = inferExpr(call->callee); // will throw if not defined
        // calleeType expected like "fn(int,int)->int"
        if (calleeType.rfind("fn(", 0) != 0)
        {
            throw std::runtime_error("Attempting to call non-function type: " + calleeType);
        }
        // parse signature
        // fn(a,b)->ret  OR fn()->void
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

    // ArrayExpr
    if (auto a = std::dynamic_pointer_cast<ArrayExpr>(expr))
    {
        if (a->elements.empty())
        {
            throw std::runtime_error("Empty array literal requires explicit type annotation in MVP");
        }
        std::string firstType = inferExpr(a->elements[0]);
        for (size_t i = 1; i < a->elements.size(); ++i)
        {
            std::string t = inferExpr(a->elements[i]);
            if (t != firstType)
            {
                throw std::runtime_error("Array literal elements must be homogeneous: " + firstType + " vs " + t);
            }
        }
        return "array<" + firstType + ">";
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
    // ArrayAssignExpr (array[index] = value)
    if (auto aa = std::dynamic_pointer_cast<ArrayAssignExpr>(expr))
    {
        std::string arrType = inferExpr(aa->array);
        if (!isArrayType(arrType))
            throw std::runtime_error("Array assignment target is not array");

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

        // rest of type checking...
        std::string elemType = arrayElementType(arrType);
        std::string valType = inferExpr(aa->value);
        if (valType != elemType)
        {
            if (!(isIntType(valType) && isFloatType(elemType)))
                throw std::runtime_error("Array assignment type mismatch: expected " + elemType + ", got " + valType);
        }

        return elemType;
    }

    throw std::runtime_error("Unhandled expression type in typechecker");
}
