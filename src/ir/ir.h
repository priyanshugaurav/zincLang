#pragma once
#include <memory>
#include <map>
#include <string>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>

#include "../ast/ast.h"

enum class ValueType {
    Int, Float, Bool, String, Null
};

struct ValueBox {
    ValueType type;
    llvm::Value* value;
};

class Codegen {
public:
    llvm::LLVMContext context;
    std::unique_ptr<llvm::Module> module;
    llvm::IRBuilder<> builder;

    std::map<std::string, llvm::AllocaInst*> namedValues;

    Codegen(const std::string& moduleName)
        : module(std::make_unique<llvm::Module>(moduleName, context)),
          builder(context) {}

    llvm::Value* codegenExpr(const ExprPtr& expr);
    void codegenStmt(const StmtPtr& stmt);
    llvm::Module* getModule() { return module.get(); }

      llvm::Value* storeNullable(llvm::AllocaInst* alloca, llvm::Value* val, bool isNull);
    llvm::Value* storeDynamicVar(const std::string& name, llvm::Value* val, ValueType type);

private:

    llvm::Type* toLLVMType(const std::string& ty);
    llvm::Value* castValue(llvm::Value* val, llvm::Type* targetTy);
    llvm::AllocaInst* createEntryBlockAlloca(llvm::Function* func, const std::string& name, llvm::Type* type);

    llvm::Value* storeDynamic(const std::string& name, llvm::Value* val, ValueType type);
};