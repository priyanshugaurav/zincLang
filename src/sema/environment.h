#pragma once
#include <string>
#include <unordered_map>
#include <memory>
#include <optional>
#include <vector>

// ----------------------------
// Symbol Information
// ----------------------------
struct Symbol {
    std::string name;
    std::string type;       // e.g., "array<int>"
    std::string elementType; // store inner type for homogeneous arrays
    std::string value;      // optional
    int arraySize = -1;     // -1 = unknown
    bool isMutable;
    bool isDynamic = false;
    bool isNullable = false;

    Symbol() : name(""), type(""), arraySize(-1), isMutable(false), isDynamic(false), isNullable(false) {}
    Symbol(const std::string &n, const std::string &t, bool mut, int arrSize = -1, bool dyn = false, bool nullable = false)
        : name(n), type(t), elementType(""), arraySize(arrSize), isMutable(mut), isDynamic(dyn), isNullable(nullable) {}
};




struct FunctionType {
    std::vector<std::string> paramTypes;
    std::string returnType;
};

// ----------------------------
// Environment (Lexical Scope)
// ----------------------------
// environment.h
class Environment
{
public:
    // constructor
    explicit Environment(std::shared_ptr<Environment> parent = nullptr);

    // Add a symbol to current scope
    // Overload to define array with size
bool define(const std::string &name, const std::string &type, bool isMutable,
            int arraySize = -1, bool dyn = false, bool nullable = false);


    bool defineFunction(const std::string &name, const FunctionType &ftype)
    {
        if (table.count(name))
            return false;

        // Store the symbol with type "fn" and immutable
        table[name] = Symbol{name, "fn", false};

        // Also store function type mapping (can be added to Environment)
        functionTypes[name] = ftype;

        return true;
    }

    // Add this to private:
    std::unordered_map<std::string, FunctionType> functionTypes;

    // Lookup symbol in current + parent scopes
    std::optional<Symbol> lookup(const std::string &name) const;

    // Assign (check mutability + type consistency)
    bool assign(const std::string& name, const std::string& type, const std::string& value);

private:
    std::unordered_map<std::string, Symbol> table;
    std::shared_ptr<Environment> parent;
};
