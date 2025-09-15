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

string IRBuilder::newTmp(){ return "%t" + to_string(++tmpCounter); }
string IRBuilder::newBBName(){ return "bb" + to_string(++bbCounter); }

Function* IRBuilder::createFunction(const string &name, Type retType){
    module->functions.push_back(make_unique<Function>(name, retType));
    curFunc = module->functions.back().get();
    // create entry block
    auto *entry = new BasicBlock("entry");
    curFunc->blocks.emplace_back(entry);
    curBB = entry;
    return curFunc;
}

BasicBlock* IRBuilder::createBlock(const string &hint){
    string n = hint.empty() ? newBBName() : hint;
    curFunc->blocks.emplace_back(make_unique<BasicBlock>(n));
    return curFunc->blocks.back().get();
}

void IRBuilder::setInsertPoint(BasicBlock* bb){ curBB = bb; }

Instruction* IRBuilder::createConst(const string &val, Type t){
    string n = newTmp();
    auto inst = make_unique<Instruction>(Instruction::Op::Const, n, t);
    inst->isConst = true;
    inst->constVal = val;
    Instruction* p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}

Instruction* IRBuilder::createAlloca(Type t, const string &hint){
    string name = hint.empty() ? newTmp() : "%" + hint;
    auto inst = make_unique<Instruction>(Instruction::Op::Alloca, name, t);
    Instruction* p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}

Instruction* IRBuilder::createLoad(Value* addr){
    string name = newTmp();
    auto inst = make_unique<Instruction>(Instruction::Op::Load, name, addr->type);
    inst->operands.push_back(addr);
    Instruction* p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}

Instruction* IRBuilder::createStore(Value* val, Value* addr){
    auto inst = make_unique<Instruction>(Instruction::Op::Store, "", Type::Void());
    inst->operands.push_back(val);
    inst->operands.push_back(addr);
    Instruction* p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}

Instruction* IRBuilder::createBinOp(Instruction::Op op, Value* a, Value* b, Type t){
    string name = newTmp();
    auto inst = make_unique<Instruction>(op, name, t);
    inst->operands.push_back(a);
    inst->operands.push_back(b);
    Instruction* p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}

Instruction* IRBuilder::createRet(Value* v){
    auto inst = make_unique<Instruction>(Instruction::Op::Ret, "", Type::Void());
    if (v) inst->operands.push_back(v);
    Instruction* p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}

Instruction* IRBuilder::createBr(BasicBlock* target){
    auto inst = make_unique<Instruction>(Instruction::Op::Br, "", Type::Void());
    inst->target = target;
    Instruction* p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}

Instruction* IRBuilder::createCondBr(Value* cond, BasicBlock* t, BasicBlock* f){
    auto inst = make_unique<Instruction>(Instruction::Op::CondBr, "", Type::Void());
    inst->operands.push_back(cond);
    inst->target = t;
    inst->target2 = f;
    Instruction* p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}

Instruction* IRBuilder::createPhi(Type t, const vector<pair<Value*, BasicBlock*>>& incoming){
    string name = newTmp();
    auto inst = make_unique<Instruction>(Instruction::Op::Phi, name, t);
    inst->phiIncoming = incoming;
    Instruction* p = inst.get();
    curBB->instrs.push_back(move(inst));
    return p;
}

// ---------------------------
// Lowering: AST -> IR
// ---------------------------

void IRBuilder::lowerFunction(const shared_ptr<FuncDecl>& fdecl){
    // For now assume functions have no typed params & return int for simplicity unless declared
    Type ret = Type::Int();
    auto f = createFunction(fdecl->name, ret);

    // insert allocas for params/locals can be added here.

    // Lower body
    // we assume fdecl->body is a BlockStmt
    if (fdecl->body) {
        lowerStatement(fdecl->body);
    }

    // ensure function ends with ret
    // naive: if last instruction not ret, add ret 0
    if (curBB->instrs.empty() || curBB->instrs.back()->op != Instruction::Op::Ret) {
        createRet(nullptr);
    }
}

Value* IRBuilder::lowerExpression(const ExprPtr &expr){
    if (!expr) return nullptr;

    // Literal
    if (auto lit = dynamic_pointer_cast<LiteralExpr>(expr)) {
        // create const instruction
        if (lit->type == "int") {
            return createConst(lit->value, Type::Int());
        } else if (lit->type == "float") {
            return createConst(lit->value, Type::Float());
        } else if (lit->type == "bool") {
            return createConst(lit->value, Type::Bool());
        } else {
            return createConst(lit->value, Type::Str());
        }
    }

    // Identifier -> we'll treat as load of an alloca named %<ident>
    if (auto id = dynamic_pointer_cast<IdentifierExpr>(expr)) {
        // find an alloca in current function blocks named "%<id>"
        // naive search: scan entry block for an alloca with that name
        BasicBlock* entry = curFunc->blocks.front().get();
        for (auto &i : entry->instrs) {
            if (i->op == Instruction::Op::Alloca && i->name == "%" + id->name) {
                return createLoad(i.get());
            }
        }
        // not found: treat as zero constant
        return createConst("0", Type::Int());
    }

    // Binary
    if (auto bin = dynamic_pointer_cast<BinaryExpr>(expr)) {
        // handle assignment handled in statement lowering; here assume pure ops
        auto lhs = lowerExpression(bin->lhs);
    auto rhs = lowerExpression(bin->rhs);
        string op = bin->op;
        if (op == "+") return createBinOp(Instruction::Op::Add, lhs, rhs, Type::Int());
        if (op == "-") return createBinOp(Instruction::Op::Sub, lhs, rhs, Type::Int());
        if (op == "*") return createBinOp(Instruction::Op::Mul, lhs, rhs, Type::Int());
        if (op == "/") return createBinOp(Instruction::Op::Div, lhs, rhs, Type::Int());
        if (op == "==" || op == "!=" || op == "<" || op == ">" || op == "<=" || op == ">=") {
            // For now produce comparison as returning bool (here represent as const 0/1 pattern)
            // Lowering comparisons to bool is left as exercise; return rhs for now
            return createBinOp(Instruction::Op::Sub, lhs, rhs, Type::Int());
        }
        if (op == "=") {
            // assignment: left must be identifier or index
            if (auto id = dynamic_pointer_cast<IdentifierExpr>(bin->lhs)) {
                // find alloca
                BasicBlock* entry = curFunc->blocks.front().get();
                Instruction* allocaInst = nullptr;
                for (auto &i : entry->instrs) {
                    if (i->op == Instruction::Op::Alloca && i->name == "%" + id->name) {
                        allocaInst = i.get();
                        break;
                    }
                }
                if (!allocaInst) {
                    // create alloca in entry
                    auto savedBB = curBB;
                    setInsertPoint(entry);
                    allocaInst = createAlloca(Type::Int(), id->name);
                    setInsertPoint(savedBB);
                }
                auto val = lowerExpression(bin->rhs);
                createStore(val, allocaInst);
                return val;
            }
            // array assign etc. not supported here
        }
    }

    // Call
    if (auto call = dynamic_pointer_cast<CallExpr>(expr)) {
        // evaluate callee (identifier)
        if (auto id = dynamic_pointer_cast<IdentifierExpr>(call->callee)) {
            // lower args
            vector<Value*> args;
            for (auto &a : call->args) args.push_back(lowerExpression(a));
            // create a call instruction (very naive)
            string name = newTmp();
            auto inst = make_unique<Instruction>(Instruction::Op::Call, name, Type::Int());
            for (auto av : args) inst->operands.push_back(av);
            auto p = inst.get();
            curBB->instrs.push_back(move(inst));
            return p;
        }
    }

    // Unary
    if (auto un = dynamic_pointer_cast<UnaryExpr>(expr)) {
        auto v = lowerExpression(un->rhs    );
        if (un->op == "-") {
            return createBinOp(Instruction::Op::Sub, createConst("0", Type::Int()), v, Type::Int());
        }
        if (un->op == "!") {
            // not: naive
            return createBinOp(Instruction::Op::Sub, createConst("1", Type::Int()), v, Type::Int());
        }
    }

    // If-expression (ternary like) -> produce blocks + phi
    if (auto ife = dynamic_pointer_cast<IfExpr>(expr)) {
        auto cond = lowerExpression(ife->condition);

        BasicBlock* thenBB = createBlock("then");
        BasicBlock* elseBB = createBlock("else");
        BasicBlock* mergeBB = createBlock("merge");

        createCondBr(cond, thenBB, elseBB);
        // then
        setInsertPoint(thenBB);
        auto thenVal = lowerExpression(ife->thenBranch);
        createBr(mergeBB);

        // else
        setInsertPoint(elseBB);
        Value* elseVal = nullptr;
        if (ife->elseBranch) elseVal = lowerExpression(ife->elseBranch);
        createBr(mergeBB);

        // merge
        setInsertPoint(mergeBB);
        vector<pair<Value*, BasicBlock*>> incoming;
        // note: last instruction of thenBB and elseBB that produced the value may be tmp in that block.
        // find most recent tmp in their last instruction:
        if (!thenBB->instrs.empty()) incoming.push_back({ thenBB->instrs.back().get(), thenBB });
        else incoming.push_back({ createConst("0", Type::Int()), thenBB });
        if (!elseBB->instrs.empty()) incoming.push_back({ elseBB->instrs.back().get(), elseBB});
        else incoming.push_back({ createConst("0", Type::Int()), elseBB });

        auto phi = createPhi(Type::Int(), incoming);
        return phi;
    }

    // Fallback
    return createConst("0", Type::Int());
}

void IRBuilder::lowerStatement(const StmtPtr &stmt){
    if (!stmt) return;

    if (auto block = dynamic_pointer_cast<BlockStmt>(stmt)) {
        for (auto &s : block->statements)
            lowerStatement(s);
        return;
    }

    if (auto exprStmt = dynamic_pointer_cast<ExprStmt>(stmt)) {
        lowerExpression(exprStmt->expr);
        return;
    }

    if (auto vdecl = dynamic_pointer_cast<VarDecl>(stmt)) {
        // allocate in entry block, initialize if present
        BasicBlock* entry = curFunc->blocks.front().get();
        Instruction* allocaInst = nullptr;
        // create alloca in entry
        auto saved = curBB;
        setInsertPoint(entry);
        allocaInst = createAlloca(Type::Int(), vdecl->name);
        setInsertPoint(saved);

        if (vdecl->initializer) {
            auto val = lowerExpression(vdecl->initializer);
            createStore(val, allocaInst);
        }
        return;
    }

    if (auto ret = dynamic_pointer_cast<ReturnStmt>(stmt)) {
        Value* v = nullptr;
        if (ret->value) v = lowerExpression(ret->value);
        createRet(v);
        return;
    }

    if (auto ifs = dynamic_pointer_cast<IfStmt>(stmt)) {
        auto cond = lowerExpression(ifs->condition);
        BasicBlock* thenBB = createBlock("then");
        BasicBlock* elseBB = createBlock("else");
        BasicBlock* mergeBB = createBlock("merge");
        createCondBr(cond, thenBB, elseBB);

        setInsertPoint(thenBB);
        lowerStatement(ifs->thenBranch);
        // ensure then ends with br to merge if not terminated by return
        if (curBB->instrs.empty() || curBB->instrs.back()->op != Instruction::Op::Br)
            createBr(mergeBB);

        setInsertPoint(elseBB);
        if (ifs->elseBranch) lowerStatement(ifs->elseBranch);
        if (curBB->instrs.empty() || curBB->instrs.back()->op != Instruction::Op::Br)
            createBr(mergeBB);

        setInsertPoint(mergeBB);
        // NOTE: if variables assigned in branches need phi nodes - omitted here (manual phi insertion in expression lowering)
        return;
    }

    if (auto ws = dynamic_pointer_cast<WhileStmt>(stmt)) {
        BasicBlock* header = createBlock("while.header");
        BasicBlock* body = createBlock("while.body");
        BasicBlock* exit = createBlock("while.exit");
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
Module* IRBuilder::build(const StmtPtr& ast) {
    // Create a new module if not already
    if (!module) {
        module = new Module("main");
    }

    // If the AST root is a block, lower all statements
    if (auto block = dynamic_pointer_cast<BlockStmt>(ast)) {
        for (auto &stmt : block->statements) {
            // If it's a function declaration, lower as function
            if (auto fdecl = dynamic_pointer_cast<FuncDecl>(stmt)) {
                lowerFunction(fdecl);
            } else {
                // Lower as normal statement in some pseudo-main function
                if (!curFunc) {
                    // create a top-level "main" function if needed
                    createFunction("main", Type::Int());
                }
                lowerStatement(stmt);
            }
        }
    } else {
        // Not a block, just a single statement
        if (!curFunc) {
            createFunction("main", Type::Int());
        }
        lowerStatement(ast);
    }

    // ensure main function ends with ret
    if (curFunc && (curFunc->blocks.back()->instrs.empty() || curFunc->blocks.back()->instrs.back()->op != Instruction::Op::Ret)) {
        setInsertPoint(curFunc->blocks.back().get());
        createRet(nullptr);
    }

    return module;
}

