#pragma once
#include <string>
#include <unordered_map>
#include <memory>
#include <optional>
#include <vector>

struct Symbol {
    std::string name;
    std::string type;       
    std::string elementType; 
    std::string value;      
    int arraySize = -1;     
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

class Environment
{
public:

    explicit Environment(std::shared_ptr<Environment> parent = nullptr);

bool define(const std::string &name, const std::string &type, bool isMutable,
            int arraySize = -1, bool dyn = false, bool nullable = false);

    bool defineFunction(const std::string &name, const FunctionType &ftype)
    {
        if (table.count(name))
            return false;

        table[name] = Symbol{name, "fn", false};

        functionTypes[name] = ftype;

        return true;
    }

    std::unordered_map<std::string, FunctionType> functionTypes;

    std::optional<Symbol> lookup(const std::string &name) const;

    bool assign(const std::string& name, const std::string& type, const std::string& value);

private:
    std::unordered_map<std::string, Symbol> table;
    std::shared_ptr<Environment> parent;
};