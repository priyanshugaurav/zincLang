#pragma once
#include "ast/ast.h"
#include <fstream>
#include <unordered_map>
#include <vector>
#include <stdexcept>
#include <string>
#include <algorithm>

namespace nasm
{
    class Codegen
    {
        std::ofstream out;
        int labelCounter = 0;
        int stackOffset = 0; // negative offsets from rbp (e.g. -8, -16)
        std::unordered_map<std::string, int> varTable;
        std::vector<std::string> dataSection;
        std::vector<std::shared_ptr<FuncDecl>> deferredFunctions;
        std::vector<std::unordered_map<std::string, std::string>> scopeStack; // for nested variable scopes
        int scopeLevel = 0;

        std::string newLabel(const std::string &prefix)
        {
            return prefix + std::to_string(labelCounter++);
        }

        // Return a memory operand like: qword [rbp-8]
        std::string slot(int off)
        {
            std::string s = "rbp";
            if (off < 0)
                s += std::to_string(off); // rbp-8
            else if (off > 0)
                s += "+" + std::to_string(off);
            return "qword [" + s + "]";
        }

        std::unordered_map<std::string, std::string> exprTypes;
        std::unordered_map<std::string, std::string> functionReturnTypes;

        void genStmtSkipNestedFunctions(const StmtPtr &stmt)
        {
            if (!stmt)
                return;

            if (auto block = std::dynamic_pointer_cast<BlockStmt>(stmt))
            {
                for (auto &s : block->statements)
                {
                    // Skip nested function declarations - they're already deferred
                    if (std::dynamic_pointer_cast<FuncDecl>(s))
                        continue;
                    genStmt(s);
                }
            }
            else if (auto f = std::dynamic_pointer_cast<FuncDecl>(stmt))
            {
                // Skip - this should have been deferred
                return;
            }
            else
            {
                // For all other statement types, use normal genStmt
                genStmt(stmt);
            }
        }

        // Escape double quotes and backslashes for db "..." emission
        static std::string escapeString(const std::string &in)
        {
            std::string out;
            out.reserve(in.size());
            for (char c : in)
            {
                if (c == '"' || c == '\\')
                {
                    out.push_back('\\');
                }
                out.push_back(c);
            }
            return out;
        }

    public:
        Codegen(const std::string &filename)
        {
            out.open(filename);
            if (!out)
                throw std::runtime_error("Could not open output file");

            dataSection.push_back("dot_char: db '.', 0"); // keep dot_char here
            // dataSection.push_back("num_buf: resb 32"); // REMOVE THIS LINE
            dataSection.push_back("minus_char: db '-', 0");
            dataSection.push_back("half_const: dq 0.5");
            dataSection.push_back("true_str: db 'true', 0");
            dataSection.push_back("false_str: db 'false', 0");
            // minus_char: db '-', 0
        }

        void emit(const std::string &s) { out << s << "\n"; }
        std::unordered_map<std::string, std::string> varTypes; // "string", "int", etc.

        std::string getExprType(const ExprPtr &expr)
        {
            if (auto lit = std::dynamic_pointer_cast<LiteralExpr>(expr))
            {
                return lit->type;
            }
            else if (auto id = std::dynamic_pointer_cast<IdentifierExpr>(expr))
            {
                auto it = varTypes.find(id->name);
                return (it != varTypes.end()) ? it->second : "int";
            }
            // ADD this case for function calls:
            else if (auto call = std::dynamic_pointer_cast<CallExpr>(expr))
            {
                if (auto callee = std::dynamic_pointer_cast<IdentifierExpr>(call->callee))
                {
                    auto it = functionReturnTypes.find(callee->name);
                    if (it != functionReturnTypes.end())
                    {
                        return it->second;
                    }
                }
                return "int"; // default fallback
            }
            else if (auto bin = std::dynamic_pointer_cast<BinaryExpr>(expr))
            {
                if (bin->op == "=")
                {
                    return getExprType(bin->rhs);
                }
                // Comparison operators return bool
                else if (bin->op == "==" || bin->op == "!=" || bin->op == "<" ||
                         bin->op == "<=" || bin->op == ">" || bin->op == ">=")
                {
                    return "bool";
                }
                else
                {
                    std::string leftType = getExprType(bin->lhs);
                    std::string rightType = getExprType(bin->rhs);

                    // If either operand is float, result is float
                    if (leftType == "float" || rightType == "float")
                        return "float";
                    else
                        return "int";
                }
            }
            else if (auto un = std::dynamic_pointer_cast<UnaryExpr>(expr))
            {
                if (un->op == "!")
                    return "bool";
                else
                    return getExprType(un->rhs);
            }

            return "int"; // default
        }
        void emitPrintBool()
        {
            std::string lbl = newLabel("bool");

            emit("    cmp rax, 0");
            emit("    je " + lbl + "_false");

            // Print "true"
            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    lea rsi, [rel true_str]");
            emit("    mov rdx, 4");
            emit("    syscall");
            emit("    jmp " + lbl + "_done");

            emit(lbl + "_false:");
            // Print "false"
            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    lea rsi, [rel false_str]");
            emit("    mov rdx, 5");
            emit("    syscall");

            emit(lbl + "_done:");
        }

        void emitPrintDouble()
        {
            std::string lbl = newLabel("dbl");

            emit("    push rbx");
            emit("    push r12");
            emit("    push r13");
            emit("    push r14");
            emit("    sub rsp, 16");
            emit("    movsd [rsp], xmm0"); // save original double

            // Handle negative numbers
            emit("    xorpd xmm2, xmm2"); // zero
            emit("    comisd xmm0, xmm2");
            emit("    jae " + lbl + "_positive");

            // Print minus sign and make positive
            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    lea rsi, [rel minus_char]");
            emit("    mov rdx, 1");
            emit("    syscall");
            emit("    movsd xmm0, [rsp]");
            emit("    mov rax, 0x8000000000000000"); // sign bit mask
            emit("    movq xmm1, rax");
            emit("    xorpd xmm0, xmm1"); // flip sign bit to make positive
            emit("    movsd [rsp], xmm0");

            emit(lbl + "_positive:");

            // Get integer part
            emit("    movsd xmm0, [rsp]");
            emit("    cvttsd2si rax, xmm0");
            emit("    mov [rsp+8], rax"); // save integer part
            emitPrintRax();

            // Print decimal point
            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    lea rsi, [rel dot_char]");
            emit("    mov rdx, 1");
            emit("    syscall");

            // Calculate fractional part
            emit("    movsd xmm0, [rsp]");  // original (positive) value
            emit("    mov rax, [rsp+8]");   // integer part
            emit("    cvtsi2sd xmm1, rax"); // convert int back to double
            emit("    subsd xmm0, xmm1");   // frac = orig - int_as_double

            // Multiply by 1000000 for 6 decimal places and round
            emit("    mov rax, 1000000");
            emit("    cvtsi2sd xmm1, rax");
            emit("    mulsd xmm0, xmm1");
            emit("    addsd xmm0, [rel half_const]"); // add 0.5 for rounding
            emit("    cvttsd2si rax, xmm0");

            // Clamp to valid range
            emit("    cmp rax, 1000000");
            emit("    jl " + lbl + "_frac_ok");
            emit("    mov rax, 999999");
            emit(lbl + "_frac_ok:");

            emit("    cmp rax, 0");
            emit("    jge " + lbl + "_frac_positive");
            emit("    mov rax, 0");
            emit(lbl + "_frac_positive:");

            // Convert fractional part to string - build from right to left
            emit("    mov rbx, rax");           // fractional part value
            emit("    lea r14, [rel num_buf]"); // start of buffer
            emit("    mov r13, r14");           // current position
            emit("    add r13, 6");             // move to end of 6-digit space
            emit("    mov byte [r13], 0");      // null terminate

            // Convert 6 digits with leading zeros
            emit("    mov rcx, 6"); // digit counter

            emit(lbl + "_digit_loop:");
            emit("    xor rdx, rdx");
            emit("    mov rax, rbx");
            emit("    mov r8, 10");
            emit("    div r8");      // rax = quotient, rdx = remainder
            emit("    add dl, '0'"); // convert digit to ASCII
            emit("    dec r13");
            emit("    mov [r13], dl"); // store digit
            emit("    mov rbx, rax");  // update value
            emit("    loop " + lbl + "_digit_loop");

            // Remove trailing zeros
            emit("    mov rsi, r14"); // start of digits
            emit("    add rsi, 5");   // point to last digit
            emit("    mov rcx, 6");   // max digits

            emit(lbl + "_trim_loop:");
            emit("    cmp rcx, 1"); // keep at least 1 digit
            emit("    jle " + lbl + "_trim_done");
            emit("    cmp byte [rsi], '0'");
            emit("    jne " + lbl + "_trim_done");
            emit("    dec rcx");
            emit("    dec rsi");
            emit("    jmp " + lbl + "_trim_loop");

            emit(lbl + "_trim_done:");
            // Print the fractional digits
            emit("    mov rax, 1");   // sys_write
            emit("    mov rdi, 1");   // stdout
            emit("    mov rsi, r14"); // buffer start
            emit("    mov rdx, rcx"); // length
            emit("    syscall");

            emit("    add rsp, 16");
            emit("    pop r14");
            emit("    pop r13");
            emit("    pop r12");
            emit("    pop rbx");
        }

        // Add this to your constructor's dataSection initialization:
        // dataSection.push_back("half_const: dq 0.5"); // for rounding
        // Add this to your constructor's dataSection initialization:
        // dataSection.push_back("half_const: dq 0.5"); // for rounding

        // Add this to your constructor's dataSection initialization:
        // dataSection.push_back("half_const: dq 0.5"); // for rounding

        // A robust convert-and-print routine that preserves callee-saved registers
        // and computes the printed length correctly. It assumes the integer to print is in RAX.
        void emitPrintRax()
        {
            std::string label = newLabel("conv");

            // check for negative
            emit("    cmp rax, 0");
            emit("    jge ." + label + "_positive");

            // save value, print '-', restore, then negate
            emit("    push rax");
            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    lea rsi, [rel minus_char]");
            emit("    mov rdx, 1");
            emit("    syscall");
            emit("    pop rax");
            emit("    neg rax");

            emit("." + label + "_positive:");

            // save registers
            emit("    push rbx");
            emit("    push r12");
            emit("    push r13");

            emit("    mov rbx, 10");
            emit("    lea r12, [rel num_buf+31]"); // end of buffer
            emit("    mov byte [r12], 0");         // null terminator
            emit("    mov r13, r12");

            emit("." + label + "_loop:");
            emit("    xor rdx, rdx");
            emit("    div rbx"); // unsigned fine (rax non-negative now)
            emit("    add dl, '0'");
            emit("    dec r13");
            emit("    mov [r13], dl");
            emit("    test rax, rax");
            emit("    jnz ." + label + "_loop");

            // write string
            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    mov rsi, r13");
            emit("    lea rdx, [rel num_buf+31]");
            emit("    sub rdx, r13"); // length
            emit("    syscall");

            // restore registers
            emit("    pop r13");
            emit("    pop r12");
            emit("    pop rbx");
        }

        // First pass: walk function body to assign stack offsets for local variables
        void collectLocals(const StmtPtr &stmt, bool isTopLevel = false)
        {
            if (!stmt)
                return;

            if (auto block = std::dynamic_pointer_cast<BlockStmt>(stmt))
            {
                for (auto &s : block->statements)
                    collectLocals(s, isTopLevel);
            }
            else if (auto v = std::dynamic_pointer_cast<VarDecl>(stmt))
        {
            if (varTable.find(v->name) == varTable.end())
            {
                stackOffset -= 8;
                varTable[v->name] = stackOffset;
            }

            if (v->initializer)
            {
                std::string inferredType = getExprType(v->initializer);
                varTypes[v->name] = inferredType;

                genExpr(v->initializer);
                int off = varTable[v->name];

                auto it = varTypes.find(v->name);
                if (it != varTypes.end())
                {
                    if (it->second == "float")
                    {
                        emit("    movq xmm0, rax");
                        emit("    movsd " + slot(off) + ", xmm0");
                    }
                    else
                    {
                        emit("    mov " + slot(off) + ", rax");
                    }
                }
                else
                {
                    emit("    mov " + slot(off) + ", rax");
                }
            }
            else
            {
                int off = varTable[v->name];
                emit("    mov rax, 0");
                emit("    mov " + slot(off) + ", rax");
            }
        }
           
            else if (auto f = std::dynamic_pointer_cast<FuncDecl>(stmt))
            {
                if (!isTopLevel)
                {
                    // This is a nested function - defer its generation
                    deferredFunctions.push_back(f);
                    // Don't recurse into the function body here
                    return;
                }
            }
            else if (auto i = std::dynamic_pointer_cast<IfStmt>(stmt))
            {
                collectLocals(i->thenBranch, isTopLevel);
                if (i->elseBranch)
                    collectLocals(i->elseBranch, isTopLevel);
            }
            else if (auto w = std::dynamic_pointer_cast<WhileStmt>(stmt))
            {
                collectLocals(w->body, isTopLevel);
            }
            else if (auto t = std::dynamic_pointer_cast<TimesStmt>(stmt))
            {
                collectLocals(t->body, isTopLevel);
            }
        }
        void generate(StmtPtr program)
    {
        // Clear any global state
        deferredFunctions.clear();
        
        // --- Text section
        emit("section .text");
        emit("global _start");
        emit("_start:");
        emit("    call main");

        // exit with main's return value
        emit("    mov rdi, rax"); // exit code = main's return
        emit("    mov rax, 60");  // syscall: exit
        emit("    syscall");

        genStmt(program);

        // --- BSS for integer printing
        emit("section .bss");
        emit("num_buf: resb 32");

        // --- Data section
        if (!dataSection.empty())
        {
            emit("section .data");
            for (auto &d : dataSection)
                emit(d);
        }
    }

        void genStmt(const StmtPtr &stmt)
        {
            if (!stmt)
                return;

            if (auto block = std::dynamic_pointer_cast<BlockStmt>(stmt))
            {
                for (auto &s : block->statements)
                    genStmt(s);
            }
            else if (auto f = std::dynamic_pointer_cast<FuncDecl>(stmt))
            {
                functionReturnTypes[f->name] = f->returnType.empty() ? "int" : f->returnType;

                // Save current state
                std::unordered_map<std::string, int> savedVarTable = varTable;
                std::unordered_map<std::string, std::string> savedVarTypes = varTypes;
                int savedStackOffset = stackOffset;
                auto savedDeferred = deferredFunctions;

                // Reset per-function state
                varTable.clear();
                varTypes.clear();
                stackOffset = 0;
                deferredFunctions.clear();

                // Add function parameters to varTable
                int paramOffset = 16; // Start after saved rbp (8) and return address (8)

                for (const auto &param : f->params)
                {
                    const std::string &paramName = param.first;
                    const std::string &paramType = param.second.empty() ? "any" : param.second;

                    varTable[paramName] = paramOffset;
                    varTypes[paramName] = paramType;
                    paramOffset += 8;
                }

                // Collect local variables (this will defer nested functions)
                collectLocals(f->body, false);

                // Emit function label and prologue
                emit(f->name + ":");
                emit("    push rbp");
                emit("    mov rbp, rsp");

                int totalLocal = -stackOffset;
                if (totalLocal % 16 != 0)
                    totalLocal += 8;

                if (totalLocal > 0)
                    emit("    sub rsp, " + std::to_string(totalLocal));

                // Generate body code (skipping nested function declarations)
                genStmtSkipNestedFunctions(f->body);

                // Handle return logic
                bool endsWithReturn = false;
                if (auto blk = std::dynamic_pointer_cast<BlockStmt>(f->body))
                {
                    if (!blk->statements.empty())
                    {
                        if (std::dynamic_pointer_cast<ReturnStmt>(blk->statements.back()))
                            endsWithReturn = true;
                    }
                }

                if (!endsWithReturn)
                {
                    bool bodyEmpty = true;
                    if (auto blk = std::dynamic_pointer_cast<BlockStmt>(f->body))
                        bodyEmpty = blk->statements.empty();

                    if (bodyEmpty)
                        emit("    mov rax, 0");
                }

                // Epilogue
                emit("    mov rsp, rbp");
                emit("    pop rbp");
                emit("    ret");

                // Now generate all the nested functions that were deferred
                for (auto &nestedFunc : deferredFunctions)
                {
                    genStmt(nestedFunc);
                }

                // Restore outer function state
                varTable = savedVarTable;
                varTypes = savedVarTypes;
                stackOffset = savedStackOffset;
                deferredFunctions = savedDeferred;
            }
            if (auto v = std::dynamic_pointer_cast<VarDecl>(stmt))
            {
                // If var wasn't preallocated, allocate now
                if (varTable.find(v->name) == varTable.end())
                {
                    stackOffset -= 8;
                    varTable[v->name] = stackOffset;
                }

                if (v->initializer)
                {
                    // Store variable type - handle different initializer types
                    if (auto litInit = std::dynamic_pointer_cast<LiteralExpr>(v->initializer))
                    {
                        if (litInit->type == "string")
                            varTypes[v->name] = "string";
                        else if (litInit->type == "int")
                            varTypes[v->name] = "int";
                        else if (litInit->type == "float")
                            varTypes[v->name] = "float";
                    }
                    else if (auto callInit = std::dynamic_pointer_cast<CallExpr>(v->initializer))
                    {
                        // Handle function call initializers
                        std::string returnType = getExprType(callInit);
                        varTypes[v->name] = returnType;
                    }
                    else
                    {
                        // For other expression types, try to infer the type
                        std::string inferredType = getExprType(v->initializer);
                        varTypes[v->name] = inferredType;
                    }

                    genExpr(v->initializer);
                    int off = varTable[v->name];

                    // Handle different storage types
                    auto it = varTypes.find(v->name);
                    if (it != varTypes.end())
                    {
                        if (it->second == "float")
                        {
                            emit("    movq xmm0, rax");
                            emit("    movsd " + slot(off) + ", xmm0");
                        }
                        else
                        {
                            // For strings and integers, store the value/pointer in rax
                            emit("    mov " + slot(off) + ", rax");
                        }
                    }
                    else
                    {
                        emit("    mov " + slot(off) + ", rax");
                    }
                }
                else
                {
                    int off = varTable[v->name];
                    emit("    mov rax, 0");
                    emit("    mov " + slot(off) + ", rax");
                }
            }

            // UPDATED print handling in ExprStmt
            else if (auto e = std::dynamic_pointer_cast<ExprStmt>(stmt))
            {
                if (auto call = std::dynamic_pointer_cast<CallExpr>(e->expr))
                {
                    if (auto id = std::dynamic_pointer_cast<IdentifierExpr>(call->callee))
                    {
                        if (id->name == "print")
                        {
                            if (call->args.size() != 1)
                                throw std::runtime_error("print() expects exactly 1 argument");

                            auto arg = call->args[0];

                            // Check what type we're printing
                            if (auto lit = std::dynamic_pointer_cast<LiteralExpr>(arg))
                            {
                                if (lit->type == "string")
                                {
                                    std::string lbl = "str_" + std::to_string(labelCounter++);
                                    std::string esc = escapeString(lit->value);
                                    dataSection.push_back(lbl + ": db \"" + esc + "\", 0");

                                    emit("    mov rax, 1");
                                    emit("    mov rdi, 1");
                                    emit("    lea rsi, [rel " + lbl + "]");
                                    emit("    mov rdx, " + std::to_string(lit->value.size()));
                                    emit("    syscall");
                                    return;
                                }
                            }
                            else if (auto idArg = std::dynamic_pointer_cast<IdentifierExpr>(arg))
                            {
                                auto it = varTypes.find(idArg->name);
                                if (it != varTypes.end() && it->second == "string")
                                {
                                    int off = varTable[idArg->name];
                                    emit("    mov rsi, " + slot(off));
                                    std::string lenLbl = newLabel("strlen");
                                    emit("    xor rcx, rcx");
                                    emit(lenLbl + "_loop:");
                                    emit("    cmp byte [rsi+rcx], 0");
                                    emit("    je " + lenLbl + "_done");
                                    emit("    inc rcx");
                                    emit("    jmp " + lenLbl + "_loop");
                                    emit(lenLbl + "_done:");
                                    emit("    mov rdx, rcx");
                                    emit("    mov rax, 1");
                                    emit("    mov rdi, 1");
                                    emit("    syscall");
                                    return;
                                }
                            }

                            // Determine the type of the expression being printed
                            std::string argType = getExprType(arg);

                            // Generate the argument
                            genExpr(arg);

                            // Print based on the determined type
                            if (argType == "float")
                            {
                                emit("    movq xmm0, rax");
                                emitPrintDouble();
                            }
                            else if (argType == "bool")
                            {
                                emitPrintBool();
                            }
                            else
                            {
                                emitPrintRax();
                            }
                            return;
                        }
                        else
                        {
                            // Handle other function calls
                            genExpr(e->expr); // This will generate the call
                            return;
                        }
                    }
                }

                // Normal expression statement
                genExpr(e->expr);
            }
            else if (auto r = std::dynamic_pointer_cast<ReturnStmt>(stmt))
            {
                if (r->value)
                    genExpr(r->value);
                else
                    emit("    mov rax, 0");

                emit("    mov rsp, rbp");
                emit("    pop rbp");
                emit("    ret");
            }
            else if (auto i = std::dynamic_pointer_cast<IfStmt>(stmt))
            {
                std::string elseLbl = newLabel(".Lelse");
                std::string endLbl = newLabel(".Lend");

                genExpr(i->condition);
                emit("    cmp rax, 0");
                emit("    je " + elseLbl);

                genStmt(i->thenBranch);
                emit("    jmp " + endLbl);

                emit(elseLbl + ":");
                if (i->elseBranch)
                    genStmt(i->elseBranch);

                emit(endLbl + ":");
            }
            else if (auto w = std::dynamic_pointer_cast<WhileStmt>(stmt))
            {
                std::string startLbl = newLabel(".Lwhile_start");
                std::string endLbl = newLabel(".Lwhile_end");

                emit(startLbl + ":");

                // Evaluate condition, result in rax
                genExpr(w->condition);

                // Compare with 0, jump to end if false
                emit("    cmp rax, 0");
                emit("    je " + endLbl);

                // Generate loop body
                genStmt(w->body);

                // Jump back to start
                emit("    jmp " + startLbl);

                emit(endLbl + ":");
            }

            else if (auto t = std::dynamic_pointer_cast<TimesStmt>(stmt))
            {
                // Generate code for the loop count
                genExpr(t->count); // rax = number of iterations

                std::string startLbl = newLabel(".Ltimes_start");
                std::string endLbl = newLabel(".Ltimes_end");

                // Save rax (iteration count) into a local variable on the stack
                stackOffset -= 8;
                int loopVarOffset = stackOffset;
                emit("    mov " + slot(loopVarOffset) + ", rax");

                emit(startLbl + ":");

                // Load remaining count
                emit("    mov rax, " + slot(loopVarOffset));
                emit("    cmp rax, 0");
                emit("    je " + endLbl);

                // Generate loop body
                genStmt(t->body);

                // Decrement counter and store back
                emit("    mov rax, " + slot(loopVarOffset));
                emit("    sub rax, 1");
                emit("    mov " + slot(loopVarOffset) + ", rax");

                // Jump back to start
                emit("    jmp " + startLbl);

                emit(endLbl + ":");

                // Restore stack
                stackOffset += 8;
            }

            // else if (auto e = std::dynamic_pointer_cast<ExprStmt>(stmt))
            // {
            //     // Handle builtin "print" calls
            //     if (auto call = std::dynamic_pointer_cast<CallExpr>(e->expr))
            //     {
            //         if (auto id = std::dynamic_pointer_cast<IdentifierExpr>(call->callee))
            //         {
            //             if (id->name == "print")
            //             {
            //                 if (call->args.size() != 1)
            //                     throw std::runtime_error("print() expects exactly 1 argument");

            //                 auto arg = call->args[0];

            //                 // literal string => same as before (compile-time known length)
            //                 if (auto lit = std::dynamic_pointer_cast<LiteralExpr>(arg))
            //                 {
            //                     if (lit->type == "string")
            //                     {
            //                         std::string lbl = "str_" + std::to_string(labelCounter++);
            //                         std::string esc = escapeString(lit->value);
            //                         dataSection.push_back(lbl + ": db \"" + esc + "\", 0");

            //                         emit("    mov rax, 1");
            //                         emit("    mov rdi, 1");
            //                         emit("    lea rsi, [rel " + lbl + "]");
            //                         emit("    mov rdx, " + std::to_string(lit->value.size()));
            //                         emit("    syscall");
            //                     }
            //                     else
            //                     {
            //                         genExpr(arg);
            //                         emitPrintRax();
            //                     }
            //                 }
            //                 // identifier: might be a string variable, handle specially
            //                 else if (auto idArg = std::dynamic_pointer_cast<IdentifierExpr>(arg))
            //                 {
            //                     // If the compiler knows this variable is a string, emit sys_write of the pointed data.
            //                     auto it = varTypes.find(idArg->name);
            //                     if (it != varTypes.end() && it->second == "string")
            //                     {
            //                         int off = varTable[idArg->name];

            //                         // Load pointer into rsi (sys_write expects buffer in RSI)
            //                         emit("    mov rsi, " + slot(off));

            //                         // Compute length at runtime: use rcx as counter
            //                         std::string lenLbl = newLabel("strlen");
            //                         emit("    xor rcx, rcx");
            //                         emit(lenLbl + "_loop:");
            //                         emit("    cmp byte [rsi+rcx], 0");
            //                         emit("    je " + lenLbl + "_done");
            //                         emit("    inc rcx");
            //                         emit("    jmp " + lenLbl + "_loop");
            //                         emit(lenLbl + "_done:");
            //                         // rcx now holds length
            //                         emit("    mov rdx, rcx");
            //                         emit("    mov rax, 1"); // sys_write
            //                         emit("    mov rdi, 1"); // stdout
            //                         emit("    syscall");
            //                     }
            //                     else if (it != varTypes.end() && it->second == "float")
            //                     {
            //                         int off = varTable[idArg->name];
            //                         emit("    movq xmm0, " + slot(off));
            //                         emitPrintDouble();
            //                     }

            //                     else
            //                     {
            //                         // Unknown type or not string: evaluate expression and print as integer
            //                         genExpr(arg);
            //                         emitPrintRax();
            //                     }
            //                 }
            //                 else
            //                 {
            //                     // Other expression kinds: evaluate and print as integer by default
            //                     genExpr(arg);
            //                     emitPrintRax();
            //                 }

            //                 return; // handled print
            //             }
            //         }
            //     }

            //     // Normal expression statement
            //     genExpr(e->expr);
            // }

            else if (auto p = std::dynamic_pointer_cast<PrintStmt>(stmt))
            {
                // Generate code to print an expression. We won't rely on an external
                // r12 accumulator here; emitPrintRax preserves callee-saved registers.

                if (auto lit = std::dynamic_pointer_cast<LiteralExpr>(p->expr))
                {
                    if (lit->type == "string")
                    {
                        std::string lbl = "str_" + std::to_string(labelCounter++);
                        std::string esc = escapeString(lit->value);
                        dataSection.push_back(lbl + ": db \"" + esc + "\", 0");

                        emit("    mov rax, 1");
                        emit("    mov rdi, 1");
                        emit("    lea rsi, [rel " + lbl + "]");
                        emit("    mov rdx, " + std::to_string(lit->value.size()));
                        emit("    syscall");
                    }
                    else
                    {
                        genExpr(p->expr); // compute rax
                        emitPrintRax();   // convert + syscall
                    }
                }
                else
                {
                    genExpr(p->expr); // compute rax
                    emitPrintRax();   // convert + syscall
                }
            }
        }

        void genExpr(const ExprPtr &expr)
        {
            if (auto lit = std::dynamic_pointer_cast<LiteralExpr>(expr))
            {
                if (lit->type == "int")
                    emit("    mov rax, " + lit->value);
                else if (lit->type == "bool")
                    emit("    mov rax, " + std::string(lit->value == "true" ? "1" : "0"));
                else if (lit->type == "string")
                {
                    std::string lbl = "str_" + std::to_string(labelCounter++);
                    std::string esc = escapeString(lit->value);
                    dataSection.push_back(lbl + ": db \"" + esc + "\", 0");
                    emit("    lea rax, [rel " + lbl + "]");
                }
                else if (lit->type == "float")
                {
                    std::string lbl = "flt_" + std::to_string(labelCounter++);
                    dataSection.push_back(lbl + ": dq " + lit->value);
                    // FIXED: Don't call emitPrintDouble here - just load the value
                    emit("    movsd xmm0, [rel " + lbl + "]");
                    emit("    movq rax, xmm0"); // Store bit pattern in rax for consistency
                }
            }
            else if (auto id = std::dynamic_pointer_cast<IdentifierExpr>(expr))
            {
                if (varTable.find(id->name) == varTable.end())
                    throw std::runtime_error("Unknown variable: " + id->name);
                int off = varTable[id->name];

                // Check if this is a float variable
                auto it = varTypes.find(id->name);
                if (it != varTypes.end() && it->second == "float")
                {
                    emit("    movsd xmm0, " + slot(off));
                    emit("    movq rax, xmm0"); // Store bit pattern in rax
                }
                else
                {
                    emit("    mov rax, " + slot(off));
                }
            }
            else if (auto bin = std::dynamic_pointer_cast<BinaryExpr>(expr))
            {
                if (bin->op == "=")
                {
                    auto id = std::dynamic_pointer_cast<IdentifierExpr>(bin->lhs);
                    if (!id)
                        throw std::runtime_error("Invalid assignment target");
                    genExpr(bin->rhs);
                    int off = varTable[id->name];

                    // Check if target is float
                    auto it = varTypes.find(id->name);
                    if (it != varTypes.end() && it->second == "float")
                    {
                        emit("    movq xmm0, rax");
                        emit("    movsd " + slot(off) + ", xmm0");
                    }
                    else
                    {
                        emit("    mov " + slot(off) + ", rax");
                    }
                }

                else
                {
                    // Check if we're dealing with float operations
                    bool leftIsFloat = false, rightIsFloat = false;

                    // Check left operand
                    if (auto leftId = std::dynamic_pointer_cast<IdentifierExpr>(bin->lhs))
                    {
                        auto it = varTypes.find(leftId->name);
                        leftIsFloat = (it != varTypes.end() && it->second == "float");
                    }
                    else if (auto leftLit = std::dynamic_pointer_cast<LiteralExpr>(bin->lhs))
                    {
                        leftIsFloat = (leftLit->type == "float");
                    }

                    // Check right operand
                    if (auto rightId = std::dynamic_pointer_cast<IdentifierExpr>(bin->rhs))
                    {
                        auto it = varTypes.find(rightId->name);
                        rightIsFloat = (it != varTypes.end() && it->second == "float");
                    }
                    else if (auto rightLit = std::dynamic_pointer_cast<LiteralExpr>(bin->rhs))
                    {
                        rightIsFloat = (rightLit->type == "float");
                    }

                    if (leftIsFloat || rightIsFloat)
                    {
                        // Floating-point arithmetic
                        genExpr(bin->lhs);
                        if (!leftIsFloat)
                        {
                            // Convert integer to float
                            emit("    cvtsi2sd xmm0, rax");
                        }
                        else
                        {
                            emit("    movq xmm0, rax");
                        }
                        emit("    sub rsp, 8");
                        emit("    movsd [rsp], xmm0");

                        genExpr(bin->rhs);
                        if (!rightIsFloat)
                        {
                            // Convert integer to float
                            emit("    cvtsi2sd xmm1, rax");
                        }
                        else
                        {
                            emit("    movq xmm1, rax");
                        }

                        emit("    movsd xmm0, [rsp]");
                        emit("    add rsp, 8");

                        if (bin->op == "+")
                            emit("    addsd xmm0, xmm1");
                        else if (bin->op == "-")
                            emit("    subsd xmm0, xmm1");
                        else if (bin->op == "*")
                            emit("    mulsd xmm0, xmm1");
                        else if (bin->op == "/")
                            emit("    divsd xmm0, xmm1");

                        emit("    movq rax, xmm0");
                    }
                    else
                    {
                        // Integer arithmetic (existing code)
                        genExpr(bin->lhs);
                        emit("    push rax");
                        genExpr(bin->rhs);
                        emit("    mov rbx, rax");
                        emit("    pop rax");

                        if (bin->op == "+")
                            emit("    add rax, rbx");
                        else if (bin->op == "-")
                            emit("    sub rax, rbx");
                        else if (bin->op == "*")
                            emit("    imul rax, rbx");
                        else if (bin->op == "/")
                        {
                            emit("    cqo");
                            emit("    idiv rbx");
                        }
                        else if (bin->op == "==")
                        {
                            emit("    cmp rax, rbx");
                            emit("    sete al");
                            emit("    movzx rax, al");
                        }
                        else if (bin->op == "!=")
                        {
                            emit("    cmp rax, rbx");
                            emit("    setne al");
                            emit("    movzx rax, al");
                        }
                        else if (bin->op == "<")
                        {
                            emit("    cmp rax, rbx");
                            emit("    setl al");
                            emit("    movzx rax, al");
                        }
                        else if (bin->op == "<=")
                        {
                            emit("    cmp rax, rbx");
                            emit("    setle al");
                            emit("    movzx rax, al");
                        }
                        else if (bin->op == ">")
                        {
                            emit("    cmp rax, rbx");
                            emit("    setg al");
                            emit("    movzx rax, al");
                        }
                        else if (bin->op == ">=")
                        {
                            emit("    cmp rax, rbx");
                            emit("    setge al");
                            emit("    movzx rax, al");
                        }
                    }
                }
            }
            else if (auto un = std::dynamic_pointer_cast<UnaryExpr>(expr))
            {
                genExpr(un->rhs);
                if (un->op == "-")
                    emit("    imul rax, -1");
                else if (un->op == "!")
                {
                    emit("    cmp rax, 0");
                    emit("    sete al");
                    emit("    movzx rax, al");
                }
            }
            else if (auto call = std::dynamic_pointer_cast<CallExpr>(expr))
            {
                // Generate arguments and push them on stack (reverse order)
                for (int i = call->args.size() - 1; i >= 0; i--)
                {
                    genExpr(call->args[i]);
                    emit("    push rax");
                }

                // Call the function
                if (auto callee = std::dynamic_pointer_cast<IdentifierExpr>(call->callee))
                {
                    emit("    call " + callee->name);

                    // Clean up arguments from stack
                    if (call->args.size() > 0)
                    {
                        emit("    add rsp, " + std::to_string(call->args.size() * 8));
                    }
                }
                else
                {
                    throw std::runtime_error("Only direct function calls supported");
                }
            }
        }
    };
}
