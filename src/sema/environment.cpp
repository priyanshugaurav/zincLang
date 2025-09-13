#include "environment.h"
#include <iostream>

Environment::Environment(std::shared_ptr<Environment> parentEnv)
    : parent(std::move(parentEnv)) {}

bool Environment::define(const std::string &name, const std::string &type, bool isMutable,
                         int arraySize, bool dyn, bool nullable)
{
    if (table.count(name)) return false;

    Symbol sym{name, type, isMutable};
    sym.isDynamic = dyn;
    sym.isNullable = nullable;
    sym.arraySize = arraySize;

    if (type.rfind("array<", 0) == 0 && type.back() == '>') {
        sym.elementType = type.substr(6, type.size() - 7); // array<int> -> int
    }

    table[name] = sym;
    return true;
}




bool Environment::assign(const std::string& name, const std::string& type, const std::string& value) {
    auto it = table.find(name);
    if (it != table.end()) {
        Symbol& sym = it->second;
        if (!sym.isMutable)
            throw std::runtime_error("Cannot assign to immutable variable '" + name + "'");

        // Allow type change if dynamic
        if (!sym.isDynamic) {
    if (sym.isNullable) {
        // allow null assignment
        if (type == "null") {
            sym.value = value;
            return true;
        }

        // allow base type assignment (e.g., int fits into int?)
        std::string baseType = sym.type;
        if (!baseType.empty() && baseType.back() == '?')
            baseType.pop_back();

        if (type != baseType)
            throw std::runtime_error("Assignment type mismatch for '" + name +
                                     "': expected " + sym.type + ", got " + type);
    } else {
        if (sym.type != type)
            throw std::runtime_error("Assignment type mismatch for '" + name +
                                     "': expected " + sym.type + ", got " + type);
    }
}


        sym.type = type;   // <-- update type if dynamic
        sym.value = value;
        return true;
    }
    if (parent)
        return parent->assign(name, type, value);
    throw std::runtime_error("Variable '" + name + "' not defined");
}



std::optional<Symbol> Environment::lookup(const std::string& name) const {
    auto it = table.find(name);
    if (it != table.end()) {
        return it->second;
    }
    if (parent) {
        return parent->lookup(name);
    }
    return std::nullopt;
}

