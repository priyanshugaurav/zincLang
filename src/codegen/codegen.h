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
        }

        void emit(const std::string &s) { out << s << "\n"; }

        // A robust convert-and-print routine that preserves callee-saved registers
        // and computes the printed length correctly. It assumes the integer to print is in RAX.
        void emitPrintRax()
        {
            std::string label = newLabel("conv");

            // preserve callee-saved registers we will use
            emit("    push rbx");
            emit("    push r12");
            emit("    push r13");

            // rax : value to print
            emit("    lea rdi, [rel num_buf+19]"); // rdi = end pointer (one past last)
            emit("    mov rbx, rax");              // rbx := value (we will divide rbx)
            emit("    xor r12, r12");              // r12 := 0 (accumulator if caller wants)

            emit("    cmp rbx, 0");
            emit("    jne ." + label + "_start");
            // zero case
            emit("    dec rdi");
            emit("    mov byte [rdi], '0'");
            emit("    mov r11, rdi"); // r11 = start
            emit("    jmp ." + label + "_done");

            emit("." + label + "_start:");
            emit("    mov r11, rdi"); // r11 = end pointer
            emit("." + label + "_loop:");
            emit("    xor rdx, rdx");
            emit("    mov rax, rbx");
            emit("    mov rcx, 10");
            emit("    div rcx"); // rax = rbx / 10, rdx = rbx % 10
            emit("    add dl, '0'");
            emit("    dec rdi");
            emit("    mov [rdi], dl");
            emit("    mov rbx, rax"); // rbx := quotient
            emit("    test rax, rax");
            emit("    jnz ." + label + "_loop");

            emit("." + label + "_done:");
            emit("    mov rsi, rdi"); // rsi = start of string
            emit("    mov rdx, r11");
            emit("    sub rdx, rdi"); // rdx = length = end - start
            emit("    mov rax, 1");   // sys_write
            emit("    mov rdi, 1");   // stdout
            emit("    syscall");

            // restore callee-saved registers
            emit("    pop r13");
            emit("    pop r12");
            emit("    pop rbx");

            // Note: syscall clobbered rax; caller should not rely on rax preserving value
        }

        // First pass: walk function body to assign stack offsets for local variables
        void collectLocals(const StmtPtr &stmt)
        {
            if (!stmt)
                return;

            if (auto block = std::dynamic_pointer_cast<BlockStmt>(stmt))
            {
                for (auto &s : block->statements)
                    collectLocals(s);
            }
            else if (auto f = std::dynamic_pointer_cast<FuncDecl>(stmt))
            {
                // nested function: ignore for now or separately handle
            }
            else if (auto v = std::dynamic_pointer_cast<VarDecl>(stmt))
            {
                // allocate 8 bytes for every local (simple model)
                stackOffset -= 8;
                varTable[v->name] = stackOffset;
                // don't recurse into initializer for allocation pass
            }
            else if (auto i = std::dynamic_pointer_cast<IfStmt>(stmt))
            {
                collectLocals(i->thenBranch);
                if (i->elseBranch)
                    collectLocals(i->elseBranch);
            }
            else if (auto e = std::dynamic_pointer_cast<ExprStmt>(stmt))
            {
                // expressions don't introduce locals
            }
            else if (auto p = std::dynamic_pointer_cast<PrintStmt>(stmt))
            {
                // print argument might have var decls only in complex languages — ignore
            }
            else if (auto r = std::dynamic_pointer_cast<ReturnStmt>(stmt))
            {
                // nothing
            }
        }

        void generate(StmtPtr program)
        {
            // --- Text section
            emit("section .text");
            emit("global _start");
            emit("_start:");
            emit("    call main");

            // exit with main’s return value
            emit("    mov rdi, rax"); // exit code = main’s return
            emit("    mov rax, 60");  // syscall: exit
            emit("    syscall");

            genStmt(program);

            // --- BSS for integer printing
            emit("section .bss");
            emit("num_buf: resb 20");

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
                // Reset per-function state
                std::unordered_map<std::string, int> savedVarTable = varTable;
                int savedStackOffset = stackOffset;

                varTable.clear();
                stackOffset = 0;

                // First pass: collect locals offsets
                collectLocals(f->body);

                // Emit function label and prologue
                emit(f->name + ":");
                emit("    push rbp");
                emit("    mov rbp, rsp");

                int totalLocal = -stackOffset; // stackOffset is negative or zero
                // Align stack to 16 bytes for ABI correctness
                if (totalLocal % 16 != 0)
                    totalLocal += 8; // make space so (rsp mod 16) == 0 after call

                if (totalLocal > 0)
                    emit("    sub rsp, " + std::to_string(totalLocal));

                // Second pass: generate body code (var initializers and statements)
                genStmt(f->body);

                // If the last statement of the function body wasn't a ReturnStmt, set rax = 0
                // If the last statement of the function body wasn't a ReturnStmt, set rax = 0
                bool endsWithReturn = false;
                if (auto blk = std::dynamic_pointer_cast<BlockStmt>(f->body))
                {
                    if (!blk->statements.empty())
                    {
                        if (std::dynamic_pointer_cast<ReturnStmt>(blk->statements.back()))
                            endsWithReturn = true;
                    }
                }

                // Only force RAX = 0 if the function body is completely empty (no statements).
                // If there are statements, leave RAX alone so the last computed expression/value
                // becomes the function's return value (convenient for REPL-like behavior).
                if (!endsWithReturn)
                {
                    bool bodyEmpty = true;
                    if (auto blk = std::dynamic_pointer_cast<BlockStmt>(f->body))
                        bodyEmpty = blk->statements.empty();

                    if (bodyEmpty)
                        emit("    mov rax, 0");
                    // otherwise: don't overwrite rax — assume last stmt left a sensible value in rax
                }

                // Epilogue
                emit("    mov rsp, rbp");
                emit("    pop rbp");
                emit("    ret");

                // restore outer function state
                varTable = savedVarTable;
                stackOffset = savedStackOffset;
            }
            else if (auto v = std::dynamic_pointer_cast<VarDecl>(stmt))
            {
                // If var wasn't preallocated (e.g., top-level or unexpected), allocate now
                if (varTable.find(v->name) == varTable.end())
                {
                    stackOffset -= 8;
                    varTable[v->name] = stackOffset;
                }

                if (v->initializer)
                {
                    genExpr(v->initializer);
                    int off = varTable[v->name];
                    emit("    mov " + slot(off) + ", rax");
                }
                else
                {
                    int off = varTable[v->name];
                    emit("    mov rax, 0");
                    emit("    mov " + slot(off) + ", rax");
                }
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
            else if (auto e = std::dynamic_pointer_cast<ExprStmt>(stmt))
{
    // Handle builtin "print" calls
    if (auto call = std::dynamic_pointer_cast<CallExpr>(e->expr))
    {
        if (auto id = std::dynamic_pointer_cast<IdentifierExpr>(call->callee))
        {
            if (id->name == "print")
            {
                if (call->args.size() != 1)
                    throw std::runtime_error("print() expects exactly 1 argument");

                auto arg = call->args[0];
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
                    }
                    else
                    {
                        genExpr(arg);
                        emitPrintRax();
                    }
                }
                else
                {
                    genExpr(arg);
                    emitPrintRax();
                }
                return; // handled print
            }
        }
    }

    // Normal expression statement
    genExpr(e->expr);
}


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
                    emit("    movq xmm0, [" + lbl + "]");
                    emit("    movq rax, xmm0");
                }
            }
            else if (auto id = std::dynamic_pointer_cast<IdentifierExpr>(expr))
            {
                if (varTable.find(id->name) == varTable.end())
                    throw std::runtime_error("Unknown variable: " + id->name);
                int off = varTable[id->name];
                emit("    mov rax, " + slot(off));
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
                    emit("    mov " + slot(off) + ", rax");
                }
                else
                {
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
                        // signed division: rax contains dividend, rbx divisor
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
            else if (auto un = std::dynamic_pointer_cast<UnaryExpr>(expr))
            {
                genExpr(un->rhs);
                if (un->op == "-")
                    emit("    neg rax");
                else if (un->op == "!")
                {
                    emit("    cmp rax, 0");
                    emit("    sete al");
                    emit("    movzx rax, al");
                }
            }
        }
    };
}
