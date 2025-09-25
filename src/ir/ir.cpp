#include "ir.h"
#include <iostream>
#include <cassert>
#include "../ast/ast.h"

llvm::Type* Codegen::toLLVMType(const std::string& ty) {
    std::string baseType = ty;
    bool isNullable = false;

    if (!ty.empty() && ty.back() == '?') {
        baseType = ty.substr(0, ty.size() - 1);
        isNullable = true;
    }

    llvm::Type* llvmTy = nullptr;
    if (baseType == "int") llvmTy = llvm::Type::getInt32Ty(context);
    else if (baseType == "float") llvmTy = llvm::Type::getDoubleTy(context);
    else if (baseType == "bool") llvmTy = llvm::Type::getInt1Ty(context);
    else if (baseType == "string") llvmTy = llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(context));
    else if (baseType == "null") llvmTy = llvm::Type::getInt32Ty(context);
    else {
        std::cerr << "Unknown type: " << ty << "\n";
        return llvm::Type::getVoidTy(context);
    }

    if (isNullable) {
        llvm::Type* nullFlagTy = llvm::Type::getInt1Ty(context);
        return llvm::StructType::get(context, {nullFlagTy, llvmTy});
    }

    return llvmTy;
}

llvm::Value* Codegen::castValue(llvm::Value* val, llvm::Type* targetTy) {
    if (!val) return nullptr;
    llvm::Type* srcTy = val->getType();
    if (srcTy == targetTy) return val;

    if (targetTy->isDoubleTy() && srcTy->isIntegerTy())
        return builder.CreateSIToFP(val, targetTy, "castsi2fp");
    if (targetTy->isIntegerTy(32) && srcTy->isDoubleTy())
        return builder.CreateFPToSI(val, targetTy, "castfp2si");
    if (targetTy->isIntegerTy(1) && srcTy->isIntegerTy(32))
        return builder.CreateICmpNE(val, llvm::ConstantInt::get(srcTy, 0), "castint2bool");
    if (targetTy->isIntegerTy(32) && srcTy->isIntegerTy(1))
        return builder.CreateZExt(val, targetTy, "castbool2int");

    std::cerr << "⚠️ Unhandled cast from " << srcTy->getTypeID()
              << " to " << targetTy->getTypeID() << "\n";
    return val;
}

llvm::AllocaInst* Codegen::createEntryBlockAlloca(llvm::Function* func, const std::string& name, llvm::Type* type) {
    if (!func) return nullptr;
    llvm::IRBuilder<> tmpB(&func->getEntryBlock(), func->getEntryBlock().begin());
    return tmpB.CreateAlloca(type, nullptr, name);
}

llvm::Value* Codegen::storeNullable(llvm::AllocaInst* alloca, llvm::Value* val, bool isNull) {
    if (!alloca || !val) return nullptr;

    auto structTy = llvm::cast<llvm::StructType>(alloca->getAllocatedType());
    llvm::Value* valPtr = builder.CreateStructGEP(structTy, alloca, 1);
    llvm::Value* nullPtr = builder.CreateStructGEP(structTy, alloca, 0);

    builder.CreateStore(val, valPtr);
    builder.CreateStore(llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), isNull ? 1 : 0), nullPtr);

    return val;
}

llvm::Value* Codegen::storeDynamicVar(const std::string& name, llvm::Value* val, ValueType type) {
    if (!val) return nullptr;

    if (!namedValues.count(name)) {
        llvm::Type* dynTy = llvm::StructType::get(
            context,
            {
                llvm::Type::getInt8Ty(context),     
                llvm::Type::getInt32Ty(context),    
                llvm::Type::getDoubleTy(context),   
                llvm::Type::getInt1Ty(context),     
                llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(context)) 
            }
        );
        llvm::AllocaInst* alloca = createEntryBlockAlloca(builder.GetInsertBlock()->getParent(), name, dynTy);
        namedValues[name] = alloca;
    }

    llvm::AllocaInst* ptr = namedValues[name];
    if (!ptr) return nullptr;

    return val;
}

llvm::Value* Codegen::codegenExpr(const ExprPtr& expr) {
    if (!expr) return nullptr;

    if (auto lit = std::dynamic_pointer_cast<LiteralExpr>(expr)) {
        if (lit->type == "int") return llvm::ConstantInt::get(context, llvm::APInt(32, std::stoi(lit->value)));
        if (lit->type == "float") return llvm::ConstantFP::get(context, llvm::APFloat(std::stod(lit->value)));
        if (lit->type == "bool") return llvm::ConstantInt::get(context, llvm::APInt(1, lit->value == "true"));
        if (lit->type == "string") return builder.CreateGlobalStringPtr(lit->value);
        if (lit->type == "null") return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(context));
        return nullptr;
    }

    if (auto id = std::dynamic_pointer_cast<IdentifierExpr>(expr)) {
    if (!namedValues.count(id->name)) return nullptr;
    llvm::AllocaInst* ptr = namedValues[id->name];
    llvm::Type* type = ptr->getAllocatedType();

    if (auto structTy = llvm::dyn_cast<llvm::StructType>(type)) {
        if (structTy->getNumContainedTypes() == 2) {

            llvm::Value* valPtr = builder.CreateStructGEP(structTy, ptr, 1);
            llvm::Type* valType = structTy->getElementType(1); 
            return builder.CreateLoad(valType, valPtr, id->name);
        }
    }

    return builder.CreateLoad(type, ptr, id->name);
}

    if (auto bin = std::dynamic_pointer_cast<BinaryExpr>(expr)) {
        if (bin->op == "=") {
            auto id = std::dynamic_pointer_cast<IdentifierExpr>(bin->lhs);
            llvm::Value* rhs = codegenExpr(bin->rhs);
            if (!id || !rhs) return nullptr;

            llvm::AllocaInst* ptr = namedValues[id->name];
            llvm::Type* type = ptr->getAllocatedType();

            if (auto structTy = llvm::dyn_cast<llvm::StructType>(type)) {
                if (structTy->getNumContainedTypes() == 2) return storeNullable(ptr, rhs, false);
            }

            return builder.CreateStore(rhs, ptr);
        }

        llvm::Value* L = codegenExpr(bin->lhs);
        llvm::Value* R = codegenExpr(bin->rhs);
        if (!L || !R) return nullptr;

        if (L->getType()->isDoubleTy() || R->getType()->isDoubleTy()) {
            if (L->getType()->isIntegerTy()) L = castValue(L, llvm::Type::getDoubleTy(context));
            if (R->getType()->isIntegerTy()) R = castValue(R, llvm::Type::getDoubleTy(context));
            if (bin->op == "+") return builder.CreateFAdd(L, R);
            if (bin->op == "-") return builder.CreateFSub(L, R);
            if (bin->op == "*") return builder.CreateFMul(L, R);
            if (bin->op == "/") return builder.CreateFDiv(L, R);
        } else {
            if (bin->op == "+") return builder.CreateAdd(L, R);
            if (bin->op == "-") return builder.CreateSub(L, R);
            if (bin->op == "*") return builder.CreateMul(L, R);
            if (bin->op == "/") return builder.CreateSDiv(L, R);
        }
    }

    return nullptr;
}

void Codegen::codegenStmt(const StmtPtr& stmt) {
    if (!stmt) return;

    if (auto vd = std::dynamic_pointer_cast<VarDecl>(stmt)) {
        llvm::Type* varTy = toLLVMType(vd->typeHint.empty() ? "int" : vd->typeHint);
        llvm::AllocaInst* alloca = createEntryBlockAlloca(builder.GetInsertBlock()->getParent(), vd->name, varTy);
        namedValues[vd->name] = alloca;

        llvm::Value* initVal = nullptr;
        if (vd->initializer) initVal = codegenExpr(vd->initializer);
        else if (auto structTy = llvm::dyn_cast<llvm::StructType>(varTy)) {
            if (structTy->getNumContainedTypes() == 2) {
                initVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
                storeNullable(alloca, initVal, true);
                return;
            }
        } else {
            initVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
        }

        if (auto structTy = llvm::dyn_cast<llvm::StructType>(varTy)) {
            if (structTy->getNumContainedTypes() == 2) storeNullable(alloca, initVal, false);
            else builder.CreateStore(initVal, alloca);
        } else builder.CreateStore(initVal, alloca);

        return;
    }

    if (auto es = std::dynamic_pointer_cast<ExprStmt>(stmt)) {
        codegenExpr(es->expr);
        return;
    }

    if (auto rs = std::dynamic_pointer_cast<ReturnStmt>(stmt)) {
        llvm::Value* val = rs->value ? codegenExpr(rs->value)
                                     : llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
        if (!builder.GetInsertBlock()->getTerminator()) builder.CreateRet(val);
        return;
    }

    if (auto block = std::dynamic_pointer_cast<BlockStmt>(stmt)) {
        for (auto& s : block->statements) codegenStmt(s);
        return;
    }

    if (auto fn = std::dynamic_pointer_cast<FuncDecl>(stmt)) {
        llvm::Type* retTy = llvm::Type::getInt32Ty(context);
        auto* funcType = llvm::FunctionType::get(retTy, false);
        auto* function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, fn->name, module.get());

        llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", function);
        builder.SetInsertPoint(entry);

        codegenStmt(fn->body);

    }
}