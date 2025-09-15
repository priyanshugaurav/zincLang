// zinc/src/ir/ir.h
#pragma once
#include <string>
#include <vector>
#include <memory>
#include <unordered_map>
#include <sstream>

#include "../ast/ast.h" // adjust path if different

namespace ir
{

    // Simple type enum (expand later)
    enum class TypeKind
    {
        VOID,
        INT,
        FLOAT,
        BOOL,
        PTR,
        STRING
    };
    struct BasicBlock;

    struct Type
    {
        TypeKind kind;
        static Type Void() { return {TypeKind::VOID}; }
        static Type Int() { return {TypeKind::INT}; }
        static Type Float() { return {TypeKind::FLOAT}; }
        static Type Bool() { return {TypeKind::BOOL}; }
        static Type Ptr() { return {TypeKind::PTR}; }
        static Type Str() { return {TypeKind::STRING}; }
        bool operator==(const Type &other) const
        {
            return this->kind == other.kind; // assuming Type has a 'kind' enum or similar
        }

        bool operator!=(const Type &other) const
        {
            return !(*this == other);
        }
        std::string str() const
        {
            switch (kind)
            {
            case TypeKind::VOID:
                return "void";
            case TypeKind::INT:
                return "i32";
            case TypeKind::FLOAT:
                return "f64";
            case TypeKind::BOOL:
                return "bool";
            case TypeKind::PTR:
                return "ptr";
            case TypeKind::STRING:
                return "string";
            }
            return "unknown";
        }
    };

    struct Value
    {
        std::string name; // "%t1" or "%x"
        Type type;
        bool isConst = false;
        std::string constVal; // textual constant (if isConst)
        Value(std::string n = "", Type t = Type::Void()) : name(std::move(n)), type(t) {}
        virtual ~Value() = default;
    };

    struct Instruction : Value
    {
        enum class Op
        {
            Const,
            Alloca,
            Load,
            Store,
            Add,
            Sub,
            Mul,
            Div,
            Ret,
            Br,
            CondBr,
            Phi,
            Call,
            CmpEq,
            CmpNe,
            CmpLt,
            CmpLe,
            CmpGt,
            CmpGe,
            Cast,
            And,
            Or,
            Not
        } op;

        std::vector<Value *> operands;
        bool isConst = false;
        std::string constVal;
        std::string callee; // for Call

        // For branches
        BasicBlock *target = nullptr;
        BasicBlock *target2 = nullptr;

        // For Phi nodes
        std::vector<std::pair<Value *, BasicBlock *>> phiIncoming;

        Instruction(Op o, const std::string &n, Type t) : Value(n, t), op(o) {}
    };

    struct BasicBlock
    {
        std::string name;
        std::vector<std::unique_ptr<Instruction>> instrs;
        BasicBlock(const std::string &n) : name(n) {}
    };
    struct Function
    {
        std::string name;
        Type retType;
        std::vector<std::pair<std::string, Type>> params;
        std::vector<std::unique_ptr<BasicBlock>> blocks;
        Function(std::string n = "", Type r = Type::Void()) : name(std::move(n)), retType(r) {}
    };

    struct Module
    {
        std::string name;
        std::vector<std::unique_ptr<Function>> functions;

        Module() = default;
        Module(const std::string &n) : name(n) {}

        // Add this:
        std::string dump() const;
    };

} // namespace ir
