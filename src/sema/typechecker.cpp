#include "typechecker.h"
#include "../runtime/builtins.h"
#include <iostream>
#include <sstream>
#include <regex>

TypeChecker::TypeChecker(std::shared_ptr<Environment> env)
    : env(std::move(env)), currentReturnType("")
{

    registerBuiltins(*this->env);

    currentScope = std::make_shared<Environment>(this->env);
}

bool TypeChecker::isNumericType(const std::string &t) const
{
    return t == "int" || t == "float" || t == "any";
}
bool TypeChecker::isIntType(const std::string &t) const { return t == "int"; }
bool TypeChecker::isFloatType(const std::string &t) const { return t == "float"; }
bool TypeChecker::isBoolType(const std::string &t) const { return t == "bool"; }
bool TypeChecker::isStringType(const std::string &t) const { return t == "string"; }
bool TypeChecker::isArrayType(const std::string &t) const
{

    if (t.rfind("array<", 0) != 0)
        return false;

    int depth = 0;
    for (size_t i = 6; i < t.size(); ++i)
    {
        if (t[i] == '<')
            depth++;
        else if (t[i] == '>')
        {
            if (depth == 0)
            {

                return (i + 1 == t.size() || t[i + 1] == '?');
            }
            depth--;
        }
    }
    return false;
}

bool TypeChecker::isAnyType(const std::string &t) const
{
    return t == "any";
}

bool TypeChecker::isArrayCompatible(const std::string &declared, const std::string &inferred) const
{
    if (declared == inferred)
        return true;

    if (isArrayType(declared) && isArrayType(inferred))
    {
        std::string dElem = arrayElementType(declared);
        std::string iElem = arrayElementType(inferred);

        if (iElem == "null" && !dElem.empty() && dElem.back() == '?')
            return true;

        if (!dElem.empty() && dElem.back() == '?' && dElem.substr(0, dElem.size() - 1) == iElem)
            return true;

        if (!iElem.empty() && iElem.back() == '?' && iElem.substr(0, iElem.size() - 1) == dElem)
            return false;

        return isArrayCompatible(dElem, iElem);
    }

    if (!declared.empty() && declared.back() == '?' &&
        declared.substr(0, declared.size() - 1) == inferred)
    {
        return true;
    }

    return false;
}

bool TypeChecker::isDynamicType(const std::string &t) const { 
    return t == "dynamic" || t == "dynamic?" || t == "any" || t == "any?"; 
}

std::string TypeChecker::arrayElementType(const std::string &arrayType) const
{

    if (arrayType == "array" || arrayType == "array?") 
        return "dynamic";
    if (arrayType == "dynamic_array" || arrayType == "dynamic_array?")
        return "dynamic";

    if (!isArrayType(arrayType)) return "";

    int depth = 0;
    size_t start = arrayType.find('<');
    if (start == std::string::npos) return "dynamic"; 

    size_t i = start + 1;
    for (; i < arrayType.size(); ++i) {
        if (arrayType[i] == '<') depth++;
        else if (arrayType[i] == '>') {
            if (depth == 0) break;
            depth--;
        }
    }

    if (i >= arrayType.size()) return "dynamic"; 

    size_t end = i;
    std::string inner = arrayType.substr(start + 1, end - (start + 1));

    if (end + 1 < arrayType.size() && arrayType[end + 1] == '?') {
        if (!inner.empty() && inner.back() != '?') {
            inner.push_back('?');
        }
    }

    return inner.empty() ? "dynamic" : inner;
}

std::string TypeChecker::unifyTypes(const std::string &a, const std::string &b)
{
    auto isNullable = [](const std::string &s)
    {
        return !s.empty() && s.back() == '?';
    };
    auto withoutNullable = [&](const std::string &s)
    {
        if (isNullable(s))
            return s.substr(0, s.size() - 1);
        return s;
    };

    if (a == b)
        return a;

    if (a == "null")
        return isNullable(b) ? b : (b + "?");
    if (b == "null")
        return isNullable(a) ? a : (a + "?");

    if (isArrayType(a) && isArrayType(b))
    {
        std::string ea = arrayElementType(a);
        std::string eb = arrayElementType(b);

        if (ea.empty() || eb.empty())
        {
            bool outerNullable = isNullable(a) || isNullable(b);
            std::string result = "array<dynamic>";
            if (outerNullable)
                result.push_back('?');
            return result;
        }

        std::string unifiedElem = unifyTypes(ea, eb);
        if (unifiedElem.empty())
            unifiedElem = "dynamic";

        bool outerNullable = isNullable(a) || isNullable(b);
        std::string result = "array<" + unifiedElem + ">";
        if (outerNullable)
            result.push_back('?');
        return result;
    }

    if (isNullable(a) || isNullable(b))
    {
        std::string baseA = withoutNullable(a);
        std::string baseB = withoutNullable(b);
        std::string unified = unifyTypes(baseA, baseB);
        if (unified.empty())
            unified = "dynamic";
        if (!isNullable(unified))
            unified.push_back('?');
        return unified;
    }

    if (isIntType(a) && isFloatType(b))
        return "float";
    if (isFloatType(a) && isIntType(b))
        return "float";

    return "dynamic";
}

void TypeChecker::check(const StmtPtr &program)
{

    if (!program)
        return;
    if (auto b = std::dynamic_pointer_cast<BlockStmt>(program))
    {
        checkBlock(b->statements);
        return;
    }

    checkStmt(program);
}

void TypeChecker::checkBlock(const std::vector<StmtPtr> &stmts)
{

    auto oldEnv = env;
    env = std::make_shared<Environment>(oldEnv);

    for (auto &s : stmts)
    {
        checkStmt(s);
    }

    env = oldEnv;
}

void TypeChecker::checkStmt(const StmtPtr &stmt)
{
    if (!stmt)
        return;

if (auto v = std::dynamic_pointer_cast<VarDecl>(stmt))
{
    bool nullable = false;
    std::string baseType = v->typeHint;
    bool isDynamic = false;

    if (!v->typeHint.empty() && v->typeHint.back() == '?') {
        nullable = true;
        baseType.pop_back();
    }

    if (baseType.empty() || baseType == "dynamic" || baseType == "any") {
        isDynamic = true;
        baseType = "dynamic";
    }

    if (baseType.find("array") == 0 && baseType.find("<dynamic>") != std::string::npos) {
        isDynamic = true;
    }

    if (!v->isMutable && nullable && !isDynamic) {
        throw std::runtime_error("Immutable variable (let) cannot be nullable unless dynamic: " + v->name);
    }

    if (v->initializer) {
        std::string initType = inferExpr(v->initializer);

        if (isDynamic) {

            baseType = initType;
        } else if (!baseType.empty()) {

            if (initType == "null") {
                if (!nullable) {
                    throw std::runtime_error("Cannot assign null to non-nullable variable '" + v->name + "'");
                }
            } else if (!isArrayCompatible(baseType, initType)) {
                throw std::runtime_error("Type mismatch for variable '" + v->name +
                                       "': expected " + baseType + ", got " + initType);
            }
        } else {

            baseType = initType;
            isDynamic = (baseType == "dynamic" || baseType.find("<dynamic>") != std::string::npos);
        }
    } else {

        if (!v->isMutable) {
            throw std::runtime_error("Immutable variable (let) '" + v->name + "' must be initialized");
        }
        if (baseType.empty()) {
            baseType = "dynamic";
            nullable = true;
            isDynamic = true;
        }
    }

    if (!env->define(v->name, baseType, v->isMutable, -1, isDynamic, nullable)) {
        throw std::runtime_error("Symbol already defined in this scope: " + v->name);
    }

    return;
}

    if (auto e = std::dynamic_pointer_cast<ExprStmt>(stmt))
    {

        inferExpr(e->expr);
        return;
    }

    if (auto r = std::dynamic_pointer_cast<ReturnStmt>(stmt))
    {
        if (r->value)
        {
            std::string t = inferExpr(r->value);

            if (currentReturnType == "__INFER_RET__")
            {

                currentReturnType = t;
                return;
            }

            if (currentReturnType.empty())
            {
                throw std::runtime_error("Return with value in a function declared void / no return type");
            }

            if (t != currentReturnType)
            {

                if (t == "any" || currentReturnType == "any")
                {

                    if (currentReturnType == "any" && t != "any")
                    {
                        currentReturnType = t; 
                    }
                    return;
                }

                if (!(isIntType(t) && isFloatType(currentReturnType)))
                {
                    throw std::runtime_error("Return type mismatch: expected " + currentReturnType + ", got " + t);
                }
            }
        }
        else
        {

            if (currentReturnType.empty())
            {

                return;
            }

            if (currentReturnType == "__INFER_RET__")
            {

                currentReturnType = "void";
                return;
            }

            if (currentReturnType != "void" && currentReturnType != "any")
            {
                throw std::runtime_error("Return without value in function that expects type " + currentReturnType);
            }
        }
        return;
    }

    if (auto b = std::dynamic_pointer_cast<BlockStmt>(stmt))
    {
        checkBlock(b->statements);
        return;
    }

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

    if (auto f = std::dynamic_pointer_cast<ForStmt>(stmt))
    {
        std::string iterType = inferExpr(f->iterable);

        if (!isArrayType(iterType) && iterType != "any" && iterType != "array<any>")
        {
            throw std::runtime_error("For loop iterable must be array, got " + iterType);
        }

        auto oldEnv = env;
        env = std::make_shared<Environment>(oldEnv);
        if (!env->define(f->iterator, "int", false)) 
        {
            throw std::runtime_error("Iterator name already used in this scope: " + f->iterator);
        }

        checkStmt(f->body);

        env = oldEnv;
        return;
    }

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

    if (auto fn = std::dynamic_pointer_cast<FuncDecl>(stmt))
    {

        std::ostringstream sig;
        sig << "fn(";
        for (size_t i = 0; i < fn->params.size(); ++i)
        {
            if (i)
                sig << ",";
            std::string ptype = fn->params[i].second.empty() ? "any" : fn->params[i].second;
            sig << ptype;
        }
        sig << ")->";

        std::string initialReturnType;
        if (fn->returnType.empty())
        {
            initialReturnType = "any"; 
        }
        else
        {
            initialReturnType = fn->returnType;
        }
        sig << initialReturnType;

        if (!env->define(fn->name, sig.str(), false))
        {
            throw std::runtime_error("Function already defined: " + fn->name);
        }

        auto oldEnv = env;
        env = std::make_shared<Environment>(oldEnv);

        for (auto &p : fn->params)
        {
            const std::string &pname = p.first;
            const std::string ptype = p.second.empty() ? "any" : p.second;
            if (!env->define(pname, ptype, false))
            {
                throw std::runtime_error("Parameter name already used in function '" + fn->name + "': " + pname);
            }
        }

        std::string oldReturn = currentReturnType;
        if (fn->returnType.empty())
            currentReturnType = "__INFER_RET__";
        else
            currentReturnType = fn->returnType;

        if (auto bodyBlock = std::dynamic_pointer_cast<BlockStmt>(fn->body))
        {
            checkBlock(bodyBlock->statements);
        }
        else
        {
            checkStmt(fn->body);
        }

        if (fn->returnType.empty())
        {
            std::string actualReturnType;
            if (currentReturnType == "__INFER_RET__")
            {
                actualReturnType = "void"; 
            }
            else
            {
                actualReturnType = currentReturnType; 
            }
        }

        env = oldEnv;
        currentReturnType = oldReturn;
        return;
    }

    throw std::runtime_error("Unhandled statement in typechecker");
}

std::string TypeChecker::inferExpr(const ExprPtr &expr)
{
    if (!expr)
        return "void";

    if (auto lit = std::dynamic_pointer_cast<LiteralExpr>(expr))
    {
        if (lit->type.empty())
            throw std::runtime_error("Literal has unknown type: " + lit->value);
        return lit->type; 
    }

    if (auto id = std::dynamic_pointer_cast<IdentifierExpr>(expr))
    {
        auto symOpt = env->lookup(id->name);
        if (!symOpt.has_value())
        {
            throw std::runtime_error("Variable not defined: " + id->name);
        }
        return symOpt->type;
    }

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

    if (auto bin = std::dynamic_pointer_cast<BinaryExpr>(expr))
    {
        const std::string &op = bin->op;

        if (op == "=")
        {
            auto lhsId = std::dynamic_pointer_cast<IdentifierExpr>(bin->lhs);
            if (!lhsId)
                throw std::runtime_error("Invalid assignment target");

            std::string rhsType = inferExpr(bin->rhs);
            auto symOpt = env->lookup(lhsId->name);
            if (!symOpt.has_value())
                throw std::runtime_error("Assignment to undefined variable: " + lhsId->name);

            if (!symOpt->isMutable)
                throw std::runtime_error("Cannot assign to immutable variable (let): " + lhsId->name);

            if (rhsType == "null")
            {

                if (!symOpt->isNullable && !symOpt->isDynamic)
                    throw std::runtime_error("Cannot assign null to non-nullable variable '" + lhsId->name + "'");

                if (symOpt->isDynamic)
                {
                    symOpt->isNullable = true; 
                    symOpt->type = "any";      
                }

                return symOpt->type; 
            }

            if (symOpt->isDynamic)
            {
                symOpt->type = rhsType;
                return symOpt->type;
            }

            if (rhsType != symOpt->type)
            {
                if (!(isIntType(rhsType) && isFloatType(symOpt->type)) &&
                    !isArrayCompatible(symOpt->type, rhsType))
                {
                    throw std::runtime_error("Assignment type mismatch for '" + lhsId->name +
                                             "': expected " + symOpt->type + ", got " + rhsType);
                }
            }

            return symOpt->type;
        }

        if (op == "+" || op == "-" || op == "*" || op == "/" || op == "%")
        {
            std::string L = inferExpr(bin->lhs);
            std::string R = inferExpr(bin->rhs);

            if (op == "+" && isStringType(L) && isStringType(R))
                return "string";

            if (isDynamicType(L) || isDynamicType(R))
            {

                if (isDynamicType(L) && !isDynamicType(R))
                {

                    if (isNumericType(R))
                        return R;
                }
                if (isDynamicType(R) && !isDynamicType(L))
                {

                    if (isNumericType(L))
                        return L;
                }

                return "dynamic";
            }

            if (!isNumericType(L) || !isNumericType(R))
                throw std::runtime_error("Arithmetic requires numeric operands, got " + L + " and " + R);

            return (isFloatType(L) || isFloatType(R)) ? "float" : "int";
        }

        if (op == "==" || op == "!=")
        {
            std::string L = inferExpr(bin->lhs);
            std::string R = inferExpr(bin->rhs);

            if (L == "any" || R == "any")
                return "bool";

            if (L == "null" || R == "null")
                return "bool";

            auto stripNullable = [](const std::string &type)
            {
                return (!type.empty() && type.back() == '?') ? type.substr(0, type.size() - 1) : type;
            };

            std::string baseL = stripNullable(L);
            std::string baseR = stripNullable(R);

            if (baseL == baseR)
                return "bool";

            if (L != R && !(isNumericType(L) && isNumericType(R)))
                throw std::runtime_error("Comparison requires same or compatible types, got " + L + " and " + R);

            return "bool";
        }

        if (op == "<" || op == "<=" || op == ">" || op == ">=")
        {
            std::string L = inferExpr(bin->lhs);
            std::string R = inferExpr(bin->rhs);

            if (L == "any" || R == "any")
                return "bool";

            if (!isNumericType(L) || !isNumericType(R))
                throw std::runtime_error("Comparison requires numeric types, got " + L + " and " + R);

            return "bool";
        }

        if (op == "&&" || op == "||")
        {
            std::string L = inferExpr(bin->lhs);
            std::string R = inferExpr(bin->rhs);

            if (!isBoolType(L) || !isBoolType(R))
                throw std::runtime_error("Logical operators require bool operands");

            return "bool";
        }

        if (op == "&" || op == "|" || op == "^" || op == "<<" || op == ">>")
        {
            std::string L = inferExpr(bin->lhs);
            std::string R = inferExpr(bin->rhs);

            if (L == "any" || R == "any")
            {
                return "any";
            }

            if (!isIntType(L) || !isIntType(R))
                throw std::runtime_error("Bitwise operators require integer operands, got " + L + " and " + R);

            return "int";
        }

        throw std::runtime_error("Unknown or unsupported binary operator: " + op);
    }

    if (auto u = std::dynamic_pointer_cast<UnaryExpr>(expr))
    {
        std::string operandType = inferExpr(u->rhs);

        if (u->op == "-")
        {
            if (!isNumericType(operandType))
                throw std::runtime_error("Unary - requires numeric type, got " + operandType);
            return operandType;
        }

        if (u->op == "!")
        {
            if (operandType != "bool")
                throw std::runtime_error("Unary ! requires bool type, got " + operandType);
            return "bool";
        }

        if (u->op == "~")
        {
            if (!isIntType(operandType))
                throw std::runtime_error("Bitwise NOT requires integer type, got " + operandType);
            return "int";
        }

        throw std::runtime_error("Unsupported unary operator: " + u->op);
    }

    if (auto call = std::dynamic_pointer_cast<CallExpr>(expr))
    {

        std::string calleeType = inferExpr(call->callee); 

        if (calleeType.rfind("fn(", 0) != 0)
        {
            throw std::runtime_error("Attempting to call non-function type: " + calleeType);
        }

        size_t p1 = calleeType.find('(');
        size_t p2 = calleeType.find(")->");
        if (p1 == std::string::npos || p2 == std::string::npos)
            throw std::runtime_error("Malformed function type: " + calleeType);
        std::string paramsStr = calleeType.substr(p1 + 1, p2 - (p1 + 1));
        std::string retStr = calleeType.substr(p2 + 3);

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
                continue; 

            if (argType != paramType)
            {
                if (!(isIntType(argType) && isFloatType(paramType)))
                    throw std::runtime_error("Function call argument " + std::to_string(i) +
                                             " type mismatch: expected " + paramType + ", got " + argType);
            }
        }

        if (retStr == "void")
            return "";
        return retStr;
    }

    if (auto idx = std::dynamic_pointer_cast<IndexExpr>(expr))
    {
        std::string arrType = inferExpr(idx->array);

        if (arrType == "dynamic" || arrType == "dynamic?")
        {
            std::string idxType = inferExpr(idx->index);
            if (!isIntType(idxType))
                throw std::runtime_error("Array index must be int, got " + idxType);
            return "dynamic"; 
        }

        if (!isArrayType(arrType) && !isDynamicType(arrType))
            throw std::runtime_error("Indexing requires array type, got " + arrType);

        std::string idxType = inferExpr(idx->index);
        if (!isIntType(idxType))
            throw std::runtime_error("Array index must be int, got " + idxType);

        if (isDynamicType(arrType))
            return "dynamic";

        std::string elemType = arrayElementType(arrType);

        if (elemType == "dynamic" || elemType == "any")
        {

            return "dynamic";
        }

        return elemType;
    }

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

    if (auto aa = std::dynamic_pointer_cast<ArrayAssignExpr>(expr))
    {

        if (auto arrId = std::dynamic_pointer_cast<IdentifierExpr>(aa->array))
        {
            auto symOpt = env->lookup(arrId->name);
            if (!symOpt.has_value())
                throw std::runtime_error("Array variable not found: " + arrId->name);

            if (!symOpt->isMutable)
                throw std::runtime_error("Cannot assign to element of immutable array (let): " + arrId->name);
        }
        else if (auto idxBase = std::dynamic_pointer_cast<IndexExpr>(aa->array))
        {

            ExprPtr cur = aa->array;
            while (auto idx = std::dynamic_pointer_cast<IndexExpr>(cur))
            {
                cur = idx->array;
            }
            if (auto baseId = std::dynamic_pointer_cast<IdentifierExpr>(cur))
            {
                auto symOpt = env->lookup(baseId->name);
                if (symOpt.has_value() && !symOpt->isMutable)
                    throw std::runtime_error("Cannot assign to element of immutable array (let): " + baseId->name);
            }
        }

        std::string idxType = inferExpr(aa->index);
        if (!isIntType(idxType))
            throw std::runtime_error("Array index must be int");

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

        std::string targetArrayType;
        if (auto idx = std::dynamic_pointer_cast<IndexExpr>(aa->array))
        {

            std::string baseArrType = inferExpr(idx->array);

            if (isAnyType(baseArrType))
            {
                targetArrayType = "any";
            }
            else
            {
                if (!isArrayType(baseArrType))
                    throw std::runtime_error("Indexing requires array type, got " + baseArrType);

                targetArrayType = arrayElementType(baseArrType);
            }
        }
        else
        {

            std::string arrType = inferExpr(aa->array);
            targetArrayType = arrType;
        }

        std::string valType = inferExpr(aa->value);

        if (isAnyType(targetArrayType) || targetArrayType == "any?")
        {

            return valType;
        }

        if (!isArrayType(targetArrayType))
            throw std::runtime_error("Array assignment target is not array");

        std::string elemType = arrayElementType(targetArrayType);

        if (isAnyType(elemType))
            return valType;

        if (valType != elemType)
        {
            if (!(isIntType(valType) && isFloatType(elemType)))
                throw std::runtime_error("Array assignment type mismatch: expected " + elemType + ", got " + valType);
        }

        return elemType;
    }

    if (auto arr = std::dynamic_pointer_cast<ArrayExpr>(expr))
    {
        if (arr->elements.empty())
            return "array<dynamic>"; 

        std::vector<std::string> elemTypes;
        for (auto &elem : arr->elements)
        {
            elemTypes.push_back(inferExpr(elem));
        }

        std::string unifiedType = elemTypes[0];

        for (size_t i = 1; i < elemTypes.size(); ++i)
        {
            unifiedType = unifyTypes(unifiedType, elemTypes[i]);
        }

        if (unifiedType.empty())
        {
            unifiedType = "dynamic";
        }

        return "array<" + unifiedType + ">";
    }

    throw std::runtime_error("Unhandled expression type in typechecker");
}