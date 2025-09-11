#include "environment.h"
#include <iostream>

Environment::Environment(std::shared_ptr<Environment> parentEnv)
    : parent(std::move(parentEnv)) {}

bool Environment::define(const std::string &name, const std::string &type, bool isMutable, int arraySize, bool isDynamic) {
    if (table.find(name) != table.end()) {
        throw std::runtime_error("Variable '" + name + "' already defined in this scope");
    }
    table.emplace(name, Symbol{name, type, isMutable, arraySize, isDynamic});
    return true;
}


bool Environment::assign(const std::string& name, const std::string& type, const std::string& value) {
    auto it = table.find(name);
    if (it != table.end()) {
        Symbol& sym = it->second;
        if (!sym.isMutable)
            throw std::runtime_error("Cannot assign to immutable variable '" + name + "'");

        // Allow type change if dynamic
        if (!sym.isDynamic && sym.type != type)
            throw std::runtime_error("Assignment type mismatch for '" + name + "': expected " + sym.type + ", got " + type);

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

