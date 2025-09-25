#include "builtins.h"
#include "../sema/environment.h"

void registerBuiltins(Environment& env) {
    env.define("print", "fn(any)->void", false);
}