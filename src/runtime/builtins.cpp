#include "builtins.h"
#include "../sema/environment.h"

void registerBuiltins(Environment& env) {
    // Register print as a callable function
    env.define("print", "fn(any)->void", false);
}