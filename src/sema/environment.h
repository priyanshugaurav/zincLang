#pragma once
#include <string>
#include <unordered_map>
#include <memory>
#include <optional>
#include <vector>

// ----------------------------
// Symbol Information
// ----------------------------
struct Symbol
{
    std::string name;
    std::string type;
    std::string value;
    bool isMutable;

    // Default constructor
    Symbol() : name(""), type(""), isMutable(false) {}

    // Existing constructor
    Symbol(const std::string &n, const std::string &t, bool mut)
        : name(n), type(t), isMutable(mut) {}
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
    bool define(const std::string &name, const std::string &type, bool isMutable);

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
