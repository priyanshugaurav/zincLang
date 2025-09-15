// zinc/src/ir/ir_builder.h
#pragma once
#include "ir.h"
#include <memory>
#include <unordered_map>

namespace ir {

class IRBuilder {
    Module* module;
    Function* curFunc = nullptr;
    BasicBlock* curBB = nullptr;
    int tmpCounter = 0;
    int bbCounter = 0;
    

public:
    IRBuilder(Module* m): module(m) {}
    std::string newTmp();
    std::string newBBName();

    Function* createFunction(const std::string &name, Type retType);
    BasicBlock* createBlock(const std::string &hint);
    void setInsertPoint(BasicBlock* bb);

    // instruction creators (return created Instruction*)
    Instruction* createConst(const std::string &val, Type t);
    Instruction* createAlloca(Type t, const std::string &hint="");
    Instruction* createLoad(Value* addr);
    Instruction* createStore(Value* val, Value* addr);
    Instruction* createBinOp(Instruction::Op op, Value* a, Value* b, Type t);
    Instruction* createRet(Value* v=nullptr);
    Instruction* createBr(BasicBlock* target);
    Instruction* createCondBr(Value* cond, BasicBlock* t, BasicBlock* f);
    Instruction* createPhi(Type t, const std::vector<std::pair<Value*, BasicBlock*>>& incoming);
    Instruction* createCast(Value* val, Type targetType);
    Instruction* createCmp(Instruction::Op cmpOp, Value* a, Value* b);

    // High-level lowering interface from AST
    void lowerFunction(const std::shared_ptr<FuncDecl>& fdecl);
    Value* lowerExpression(const ExprPtr &expr);
    void lowerStatement(const StmtPtr &stmt);
    Module* build(const StmtPtr& ast);
};

} // namespace ir
