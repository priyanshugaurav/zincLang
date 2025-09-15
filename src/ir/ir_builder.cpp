// zinc/src/ir/ir_builder.cpp
#include "ir_builder.h"
#include <iostream>
#include <memory>
#include <cassert>

using namespace ir;
using namespace std;

// helpers for AST nodes - adjust if your AST uses different names/paths
// We assume the AST pointer typedefs used in parser: ExprPtr and StmtPtr
// and classes like FuncDecl, VarDecl, BinaryExpr, LiteralExpr, IdentifierExpr, IfStmt, IfExpr, WhileStmt, ReturnStmt, BlockStmt, ExprStmt, CallExpr, UnaryExpr

string IRBuilder::newTmp() { return "%t" + to_string(++tmpCounter); }
string IRBuilder::newBBName() { return "bb" + to_string(++bbCounter); }

Function *IRBuilder::createFunction(const string &name, Type retType)
{
    module->functions.push_back(make_unique<Function>(name, retType));
    curFunc = module->functions.back().get();
    // create entry block
    auto *entry = new BasicBlock("entry");
    curFunc->blocks.emplace_back(entry);
    curBB = entry;
    return curFunc;
}

BasicBlock *IRBuilder::createBlock(const string &hint)
{
    string n = hint.empty() ? newBBName() : hint;
    curFunc->blocks.emplace_back(make_unique<BasicBlock>(n));
    return curFunc->blocks.back().get();
}

void IRBuilder::setInsertPoint(BasicBlock *bb) { curBB = bb; }

Instruction *IRBuilder::createConst(const string &val, Type t)
{
    string n = newTmp();
    auto inst = make_unique<Instruction>(Instruction::Op::Const, n, t);
    inst->isConst = true;
    inst->constVal = val;
    Instruction *p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}

Instruction *IRBuilder::createAlloca(Type t, const string &hint)
{
    string name = hint.empty() ? newTmp() : "%" + hint;
    auto inst = make_unique<Instruction>(Instruction::Op::Alloca, name, t);
    Instruction *p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}

Instruction *IRBuilder::createLoad(Value *addr)
{
    string name = newTmp();
    auto inst = make_unique<Instruction>(Instruction::Op::Load, name, addr->type);
    inst->operands.push_back(addr);
    Instruction *p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}

Instruction *IRBuilder::createStore(Value *val, Value *addr)
{
    auto inst = make_unique<Instruction>(Instruction::Op::Store, "", Type::Void());
    inst->operands.push_back(val);
    inst->operands.push_back(addr);
    Instruction *p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}

Instruction *IRBuilder::createBinOp(Instruction::Op op, Value *a, Value *b, Type t)
{
    string name = newTmp();
    auto inst = make_unique<Instruction>(op, name, t);
    inst->operands.push_back(a);
    inst->operands.push_back(b);
    Instruction *p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}

Instruction *IRBuilder::createRet(Value *v)
{
    auto inst = make_unique<Instruction>(Instruction::Op::Ret, "", Type::Void());
    if (v)
        inst->operands.push_back(v);
    Instruction *p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}

Instruction *IRBuilder::createBr(BasicBlock *target)
{
    auto inst = make_unique<Instruction>(Instruction::Op::Br, "", Type::Void());
    inst->target = target;
    Instruction *p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}

Instruction *IRBuilder::createCondBr(Value *cond, BasicBlock *t, BasicBlock *f)
{
    // ensure cond is boolean-ish: if not, insert a cast before creating the condbr
    if (cond && cond->type != Type::Bool()) {
        // insert a cast in the current BB (this will create a tmp boolean)
        cond = createCast(cond, Type::Bool());
    }

    auto inst = make_unique<Instruction>(Instruction::Op::CondBr, "", Type::Void());
    inst->operands.push_back(cond);
    inst->target = t;
    inst->target2 = f;
    Instruction *p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}


Instruction *IRBuilder::createPhi(Type t, const vector<pair<Value *, BasicBlock *>> &incoming)
{
    string name = newTmp();
    auto inst = make_unique<Instruction>(Instruction::Op::Phi, name, t);
    inst->phiIncoming = incoming;
    Instruction *p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}

Instruction* IRBuilder::createCast(Value* val, Type targetType){
    string name = newTmp();
    auto inst = make_unique<Instruction>(Instruction::Op::Cast, name, targetType);
    inst->operands.push_back(val);
    Instruction* p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}


Instruction* IRBuilder::createCmp(Instruction::Op cmpOp, Value* a, Value* b) {
    string name = newTmp();
    auto inst = make_unique<Instruction>(cmpOp, name, Type::Bool());
    inst->operands.push_back(a);
    inst->operands.push_back(b);
    Instruction* p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}







// ---------------------------
// Lowering: AST -> IR
// ---------------------------

void IRBuilder::lowerFunction(const shared_ptr<FuncDecl> &fdecl)
{
    // For now assume functions have no typed params & return int for simplicity unless declared
    Type ret = Type::Int();
    auto f = createFunction(fdecl->name, ret);

    // insert allocas for params/locals can be added here.

    // Lower body
    // we assume fdecl->body is a BlockStmt
    if (fdecl->body)
    {
        lowerStatement(fdecl->body);
    }

    // ensure function ends with ret
    // naive: if last instruction not ret, add ret 0
    if (curBB->instrs.empty() || curBB->instrs.back()->op != Instruction::Op::Ret)
{
    // default return 0 for int functions
    if (curFunc->retType == Type::Int()) {
        auto zero = createConst("0", Type::Int());
        createRet(zero);
    } else {
        createRet(nullptr);
    }
}

}

Value *IRBuilder::lowerExpression(const ExprPtr &expr)
{
    if (!expr)
        return nullptr;

    // Literal
    if (auto lit = dynamic_pointer_cast<LiteralExpr>(expr))
    {
        // create const instruction
        if (lit->type == "int")
        {
            return createConst(lit->value, Type::Int());
        }
        else if (lit->type == "float")
        {
            return createConst(lit->value, Type::Float());
        }
        else if (lit->type == "bool")
        {
            return createConst(lit->value, Type::Bool());
        }
        else
        {
            return createConst(lit->value, Type::Str());
        }
    }

    // Identifier -> we'll treat as load of an alloca named %<ident>
    if (auto id = dynamic_pointer_cast<IdentifierExpr>(expr))
    {
        // find an alloca in current function blocks named "%<id>"
        // naive search: scan entry block for an alloca with that name
        BasicBlock *entry = curFunc->blocks.front().get();
        for (auto &i : entry->instrs)
        {
            if (i->op == Instruction::Op::Alloca && i->name == "%" + id->name)
            {
                return createLoad(i.get());
            }
        }
        // not found: treat as zero constant
        return createConst("0", Type::Int());
    }

    // -------------------
// Binary
// -------------------
if (auto bin = dynamic_pointer_cast<BinaryExpr>(expr)) {
    // handle assignment handled in statement lowering; here assume pure ops
    auto lhs = lowerExpression(bin->lhs);
    auto rhs = lowerExpression(bin->rhs);
    string op = bin->op;

    // arithmetic
    if (op == "+") return createBinOp(Instruction::Op::Add, lhs, rhs, lhs->type);
    if (op == "-") return createBinOp(Instruction::Op::Sub, lhs, rhs, lhs->type);
    if (op == "*") return createBinOp(Instruction::Op::Mul, lhs, rhs, lhs->type);
    if (op == "/") return createBinOp(Instruction::Op::Div, lhs, rhs, lhs->type);

    // comparisons -> produce bool results
if (op == ">")  return createCmp(Instruction::Op::CmpGt, lhs, rhs);
if (op == "<")  return createCmp(Instruction::Op::CmpLt, lhs, rhs);
if (op == "==") return createCmp(Instruction::Op::CmpEq, lhs, rhs);
if (op == "!=") return createCmp(Instruction::Op::CmpNe, lhs, rhs);
if (op == ">=") return createCmp(Instruction::Op::CmpGe, lhs, rhs);
if (op == "<=") return createCmp(Instruction::Op::CmpLe, lhs, rhs);


    // logical ops (non-short-circuited boolean ops)
    if (op == "&&") return createBinOp(Instruction::Op::And, lhs, rhs, Type::Bool());
    if (op == "||") return createBinOp(Instruction::Op::Or,  lhs, rhs, Type::Bool());

    // assignment
    if (op == "=") {
        if (auto id = dynamic_pointer_cast<IdentifierExpr>(bin->lhs)) {
            BasicBlock* entry = curFunc->blocks.front().get();
            Instruction* allocaInst = nullptr;
            for (auto &i : entry->instrs) {
                if (i->op == Instruction::Op::Alloca && i->name == "%" + id->name) {
                    allocaInst = i.get();
                    break;
                }
            }
            if (!allocaInst) {
                auto savedBB = curBB;
                setInsertPoint(entry);
                allocaInst = createAlloca(Type::Int(), id->name);
                setInsertPoint(savedBB);
            }
            auto val = lowerExpression(bin->rhs);
            if (val->type != allocaInst->type) {
                val = createCast(val, allocaInst->type);
            }
            createStore(val, allocaInst);
            return val;
        }
    }
}

// -------------------
// Unary
// -------------------
if (auto un = dynamic_pointer_cast<UnaryExpr>(expr)) {
    auto v = lowerExpression(un->rhs);
    if (un->op == "-") {
        // arithmetic negation: 0 - v (preserving type)
        return createBinOp(Instruction::Op::Sub, createConst("0", v->type), v, v->type);
    }
    if (un->op == "!") {
        // boolean not: emit Not op which should accept a bool and return bool
        // if v is not bool, cast it to bool first (non-zero -> true)
        Value* boolv = v;
        if (v->type != Type::Bool()) {
            // cast to bool (non-zero => true)
            boolv = createCast(v, Type::Bool());
        }
        // create unary Not (we model it as a binop with rhs ignored if your IR expects one)
        // Prefer an Op::Not that takes one operand; if your IR needs a binary op, adapt accordingly.
        string name = newTmp();
        auto inst = make_unique<Instruction>(Instruction::Op::Not, name, Type::Bool());
        inst->operands.push_back(boolv);
        Instruction* p = inst.get();
        curBB->instrs.push_back(move(inst));
        return p;
    }
}

    // Call
    if (auto call = dynamic_pointer_cast<CallExpr>(expr)) {
    if (auto id = dynamic_pointer_cast<IdentifierExpr>(call->callee)) {
        vector<Value*> args;
        for (auto &a : call->args) args.push_back(lowerExpression(a));
        string name = newTmp();
        auto inst = make_unique<Instruction>(Instruction::Op::Call, name, Type::Int());
        inst->callee = id->name; // <-- set callee name (ensure Instruction has this field)
        for (auto av : args) inst->operands.push_back(av);
        auto p = inst.get();
        curBB->instrs.push_back(move(inst));
        return p;
    }
}


    // Unary
    if (auto un = dynamic_pointer_cast<UnaryExpr>(expr))
    {
        auto v = lowerExpression(un->rhs);
        if (un->op == "-")
        {
            return createBinOp(Instruction::Op::Sub, createConst("0", Type::Int()), v, Type::Int());
        }
        if (un->op == "!")
        {
            // not: naive
            return createBinOp(Instruction::Op::Sub, createConst("1", Type::Int()), v, Type::Int());
        }
    }

    // If-expression (ternary like) -> produce blocks + phi
if (auto ife = dynamic_pointer_cast<IfExpr>(expr)) {
    // lower condition and ensure bool
    auto cond = lowerExpression(ife->condition);
    if (!cond) cond = createConst("0", Type::Bool());
    if (cond->type != Type::Bool()) cond = createCast(cond, Type::Bool());

    // create unique blocks
    BasicBlock* thenBB  = createBlock("");
    BasicBlock* elseBB  = createBlock("");
    BasicBlock* mergeBB = createBlock("");

    // branch on condition
    createCondBr(cond, thenBB, elseBB);

    // LOWER THEN
    setInsertPoint(thenBB);
    Value* thenVal = nullptr;
    if (ife->thenBranch) thenVal = lowerExpression(ife->thenBranch);
    // If the then-branch didn't produce a value, use a default const of int 0
    if (!thenVal) thenVal = createConst("0", Type::Int());
    // ensure terminator
    if (curBB->instrs.empty() || (curBB->instrs.back()->op != Instruction::Op::Br &&
                                  curBB->instrs.back()->op != Instruction::Op::Ret &&
                                  curBB->instrs.back()->op != Instruction::Op::CondBr)) {
        createBr(mergeBB);
    }
    BasicBlock* thenProducedBB = curBB; // block that produced thenVal

    // LOWER ELSE
    setInsertPoint(elseBB);
    Value* elseVal = nullptr;
    if (ife->elseBranch) elseVal = lowerExpression(ife->elseBranch);
    if (!elseVal) elseVal = createConst("0", Type::Int());
    if (curBB->instrs.empty() || (curBB->instrs.back()->op != Instruction::Op::Br &&
                                  curBB->instrs.back()->op != Instruction::Op::Ret &&
                                  curBB->instrs.back()->op != Instruction::Op::CondBr)) {
        createBr(mergeBB);
    }
    BasicBlock* elseProducedBB = curBB; // block that produced elseVal

    // MERGE: pick phi type (prefer thenVal type, else elseVal)
    setInsertPoint(mergeBB);
    Type phiType = thenVal->type;
    if (phiType != elseVal->type) {
        // simple coercion strategy: if one is float promote to float, else cast others to int
        if (phiType == Type::Float() || elseVal->type == Type::Float()) {
            thenVal = createCast(thenVal, Type::Float());
            elseVal = createCast(elseVal, Type::Float());
            phiType = Type::Float();
        } else {
            thenVal = createCast(thenVal, Type::Int());
            elseVal = createCast(elseVal, Type::Int());
            phiType = Type::Int();
        }
    }

    vector<pair<Value*, BasicBlock*>> incoming;
    incoming.push_back({ thenVal, thenProducedBB });
    incoming.push_back({ elseVal, elseProducedBB });

    auto phi = createPhi(phiType, incoming);
    return phi;
}

    // Fallback
    return createConst("0", Type::Int());
}

void IRBuilder::lowerStatement(const StmtPtr &stmt)
{
    if (!stmt)
        return;

    if (auto block = dynamic_pointer_cast<BlockStmt>(stmt))
    {
        for (auto &s : block->statements)
            lowerStatement(s);
        return;
    }

    if (auto exprStmt = dynamic_pointer_cast<ExprStmt>(stmt))
    {
        lowerExpression(exprStmt->expr);
        return;
    }

    if (auto vdecl = dynamic_pointer_cast<VarDecl>(stmt))
    {
        BasicBlock *entry = curFunc->blocks.front().get();
        Instruction *allocaInst = nullptr;

        auto saved = curBB;
        setInsertPoint(entry);

        // Use initializer type if exists, otherwise fall back to declared type
        Type varType = Type::Int();
        Value* initVal = nullptr;
if (vdecl->initializer) {
    initVal = lowerExpression(vdecl->initializer);
    varType = initVal->type;
}
allocaInst = createAlloca(varType, vdecl->name);
setInsertPoint(saved);

if (initVal) {
    // Note: initVal was produced earlier and can be stored directly
    if (initVal->type != allocaInst->type) {
        initVal = createCast(initVal, allocaInst->type);
    }
    createStore(initVal, allocaInst);
}

        return;
    }

    if (auto ret = dynamic_pointer_cast<ReturnStmt>(stmt))
    {
        Value *v = nullptr;
        if (ret->value)
            v = lowerExpression(ret->value);
        createRet(v);
        return;
    }

if (auto ifs = dynamic_pointer_cast<IfStmt>(stmt)) {
    
    // Lower condition and ensure it's a boolean
    auto condVal = lowerExpression(ifs->condition);
    if (!condVal) condVal = createConst("0", Type::Bool());
    if (condVal->type != Type::Bool()) {
        condVal = createCast(condVal, Type::Bool());
    }

    // Create *unique* blocks (pass empty hint so builder names them uniquely)
    BasicBlock* thenBB  = createBlock("");
    BasicBlock* elseBB  = createBlock("");
    BasicBlock* mergeBB = createBlock("");

    // Branch on condition
    createCondBr(condVal, thenBB, elseBB);

    // Then branch
    setInsertPoint(thenBB);
    lowerStatement(ifs->thenBranch);
    // If then-branch didn't already end with terminator, branch to merge
    if (curBB->instrs.empty() || (curBB->instrs.back()->op != Instruction::Op::Br &&
                                  curBB->instrs.back()->op != Instruction::Op::Ret &&
                                  curBB->instrs.back()->op != Instruction::Op::CondBr)) {
        createBr(mergeBB);
    }

    // Else branch
    setInsertPoint(elseBB);
    if (ifs->elseBranch) {
        lowerStatement(ifs->elseBranch);
    }
    if (curBB->instrs.empty() || (curBB->instrs.back()->op != Instruction::Op::Br &&
                                  curBB->instrs.back()->op != Instruction::Op::Ret &&
                                  curBB->instrs.back()->op != Instruction::Op::CondBr)) {
        createBr(mergeBB);
    }

    // Continue at merge
    setInsertPoint(mergeBB);
    return;
}

    if (auto ws = dynamic_pointer_cast<WhileStmt>(stmt))
    {
        BasicBlock *header = createBlock("while.header");
        BasicBlock *body = createBlock("while.body");
        BasicBlock *exit = createBlock("while.exit");
        // branch to header
        createBr(header);
        setInsertPoint(header);
        auto cond = lowerExpression(ws->condition);
        createCondBr(cond, body, exit);

        setInsertPoint(body);
        lowerStatement(ws->body);
        if (curBB->instrs.empty() || curBB->instrs.back()->op != Instruction::Op::Br)
            createBr(header);

        setInsertPoint(exit);
        return;
    }

    // other statements not handled yet
}

// ---------------------------
// Build IR from AST
// ---------------------------
Module *IRBuilder::build(const StmtPtr &ast)
{
    // Create a new module if not already
    if (!module)
    {
        module = new Module("main");
    }

    // If the AST root is a block, lower all statements
    if (auto block = dynamic_pointer_cast<BlockStmt>(ast))
    {
        for (auto &stmt : block->statements)
        {
            // If it's a function declaration, lower as function
            if (auto fdecl = dynamic_pointer_cast<FuncDecl>(stmt))
            {
                lowerFunction(fdecl);
            }
            else
            {
                // Lower as normal statement in some pseudo-main function
                if (!curFunc)
                {
                    // create a top-level "main" function if needed
                    createFunction("main", Type::Int());
                }
                lowerStatement(stmt);
            }
        }
    }
    else
    {
        // Not a block, just a single statement
        if (!curFunc)
        {
            createFunction("main", Type::Int());
        }
        lowerStatement(ast);
    }

    // ensure main function ends with ret
    if (curFunc && (curFunc->blocks.back()->instrs.empty() || curFunc->blocks.back()->instrs.back()->op != Instruction::Op::Ret))
    {
        setInsertPoint(curFunc->blocks.back().get());
        createRet(nullptr);
    }

    return module;
}
