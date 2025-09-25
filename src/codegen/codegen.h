#pragma once
#include "ast/ast.h"
#include <fstream>
#include <unordered_map>
#include <vector>
#include <stdexcept>
#include <string>
#include <algorithm>
#include <cstdint>

namespace nasm
{
    class Codegen
    {
        std::ofstream out;
        int labelCounter = 0;
        int stackOffset = 0; 
        std::unordered_map<std::string, int> varTable;
        std::vector<std::string> dataSection;
        std::vector<std::shared_ptr<FuncDecl>> deferredFunctions;
        std::vector<std::unordered_map<std::string, std::string>> scopeStack; 
        int scopeLevel = 0;

        static constexpr int64_t NULL_VALUE = 0x8000000000000000LL; 

        std::unordered_map<std::string, std::shared_ptr<TypeInfo>> varTypeInfo; 
        std::unordered_map<std::string, int> arrayDimensions;                   
        std::unordered_map<std::string, std::vector<int>> arrayBounds;          

        std::unordered_map<std::string, std::vector<std::string>> arrayElementTypes;
        bool errorHandlersEmitted = false;

        void storeArrayElement(const std::string &arrayVar, int index, const std::string &elemType)
        {
            if (arrayElementTypes.find(arrayVar) == arrayElementTypes.end())
            {
                arrayElementTypes[arrayVar] = std::vector<std::string>();
            }
            if (static_cast<size_t>(index) >= arrayElementTypes[arrayVar].size())
            {
                arrayElementTypes[arrayVar].resize(index + 1, "int");
            }
            arrayElementTypes[arrayVar][index] = elemType;
        }

        std::string getArrayElementType(const std::string &arrayVar, int index)
        {
            auto it = arrayElementTypes.find(arrayVar);
            if (it != arrayElementTypes.end() && static_cast<size_t>(index) < it->second.size())
            {
                return it->second[index];
            }
            return "int"; 
        }

        std::string newLabel(const std::string &prefix)
        {
            return prefix + std::to_string(labelCounter++);
        }

        bool isStringComparison(const ExprPtr &lhs, const ExprPtr &rhs)
        {

            std::string leftType = getExprType(lhs);
            std::string rightType = getExprType(rhs);

            return (leftType == "string" && rightType == "string");
        }

        std::string slot(int off)
        {
            std::string s = "rbp";
            if (off < 0)
                s += std::to_string(off); 
            else if (off > 0)
                s += "+" + std::to_string(off);
            return "qword [" + s + "]";
        }

        std::unordered_map<std::string, std::string> exprTypes;
        std::unordered_map<std::string, std::string> functionReturnTypes;

        std::unordered_map<std::string, bool> varNullable;

        void genStmtSkipNestedFunctions(const StmtPtr &stmt)
        {
            if (!stmt)
                return;

            if (auto block = std::dynamic_pointer_cast<BlockStmt>(stmt))
            {
                for (auto &s : block->statements)
                {

                    if (std::dynamic_pointer_cast<FuncDecl>(s))
                        continue;
                    genStmt(s);
                }
            }
            else if (auto f = std::dynamic_pointer_cast<FuncDecl>(stmt))
            {

                return;
            }
            else
            {

                genStmt(stmt);
            }
        }

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

        bool isNullValue(int64_t value) const
        {
            return value == NULL_VALUE;
        }

        void emitErrorHandlers()
        {
            if (errorHandlersEmitted)
                return;
            errorHandlersEmitted = true;

            emit("array_bounds_error:");
            emit("    mov rax, 1");
            emit("    mov rdi, 2"); 
            emit("    lea rsi, [rel array_bounds_msg]");
            emit("    mov rdx, 26");
            emit("    syscall");
            emit("    mov rdi, 1");  
            emit("    mov rax, 60"); 
            emit("    syscall");

            emit("null_deref_error:");
            emit("    mov rax, 1");
            emit("    mov rdi, 2");
            emit("    lea rsi, [rel null_deref_msg]");
            emit("    mov rdx, 23");
            emit("    syscall");
            emit("    mov rdi, 1");
            emit("    mov rax, 60");
            emit("    syscall");
        }

        void emitArrayBoundsCheck(const std::string &arrayReg, const std::string &indexReg)
        {
            std::string okLabel = newLabel("bounds_ok");

            emit("    cmp " + indexReg + ", 0");
            emit("    jl array_bounds_error");

            emit("    cmp " + indexReg + ", qword [" + arrayReg + "]");
            emit("    jge array_bounds_error");
            emit("    jmp " + okLabel);

            emit(okLabel + ":");
        }

        void emitArrayAllocation(int size, const std::string &elemType)
        {

            int totalBytes = 8 + (size * 8);

            emit("    mov rax, 9");                             
            emit("    mov rdi, 0");                             
            emit("    mov rsi, " + std::to_string(totalBytes)); 
            emit("    mov rdx, 3");                             
            emit("    mov r10, 34");                            
            emit("    mov r8, -1");                             
            emit("    mov r9, 0");                              
            emit("    syscall");

            emit("    mov qword [rax], " + std::to_string(size));

        }

        void emitNullCheck(const std::string &reg)
        {
            std::string okLabel = newLabel("null_ok");

            emit("    mov rbx, " + std::to_string(NULL_VALUE));
            emit("    cmp " + reg + ", rbx");
            emit("    jne " + okLabel);

            emit("    jmp null_deref_error");

            emit(okLabel + ":");
        }

    public:
        Codegen(const std::string &filename)
        {
            out.open(filename);
            if (!out)
                throw std::runtime_error("Could not open output file");

            dataSection.push_back("dot_char: db '.', 0");
            dataSection.push_back("minus_char: db '-', 0");
            dataSection.push_back("half_const: dq 0.5");
            dataSection.push_back("true_str: db 'true', 0");
            dataSection.push_back("false_str: db 'false', 0");

            dataSection.push_back("null_str: db 'null', 0");

            dataSection.push_back("open_bracket: db '[', 0");
            dataSection.push_back("close_bracket: db ']', 0");
            dataSection.push_back("comma_space: db ', ', 0");
            dataSection.push_back("array_bounds_msg: db 'Array index out of bounds', 10, 0");
            dataSection.push_back("null_deref_msg: db 'Null pointer dereference', 10, 0");
        }

        void handleArrayElementPrint(ExprPtr arg)
        {
            if (auto idx = std::dynamic_pointer_cast<IndexExpr>(arg))
            {

                genExpr(idx->array);
                emitNullCheck("rax");

                emit("    push rax");
                genExpr(idx->index);
                emit("    mov rbx, rax");
                emit("    pop rax");

                emitArrayBoundsCheck("rax", "rbx");

                emit("    push rax");
                emit("    push rbx");
                emit("    mov rcx, qword [rax + 8]"); 
                emit("    imul rbx, 8");
                emit("    add rcx, rbx");
                emit("    mov rdx, qword [rcx]"); 
                emit("    pop rbx");
                emit("    pop rax");

                emit("    imul rbx, 8");
                emit("    add rbx, 16");
                emit("    add rax, rbx");
                emit("    mov rax, qword [rax]");

                std::string floatLbl = newLabel("print_float");
                std::string stringLbl = newLabel("print_string");
                std::string boolLbl = newLabel("print_bool");
                std::string intLbl = newLabel("print_int");
                std::string arrayLbl = newLabel("print_array"); 
                std::string nullLbl = newLabel("print_null");   
                std::string doneLbl = newLabel("print_done");

                emit("    cmp rdx, 0");
                emit("    je " + intLbl);
                emit("    cmp rdx, 1");
                emit("    je " + floatLbl);
                emit("    cmp rdx, 2");
                emit("    je " + stringLbl);
                emit("    cmp rdx, 3");
                emit("    je " + boolLbl);
                emit("    cmp rdx, 4");
                emit("    je " + arrayLbl); 
                emit("    jmp " + intLbl);  

                emit(intLbl + ":");

                emit("    mov rbx, " + std::to_string(NULL_VALUE));
                emit("    cmp rax, rbx");
                emit("    je " + nullLbl);
                emitPrintRax();
                emit("    jmp " + doneLbl);

                emit(floatLbl + ":");

                emit("    mov rbx, " + std::to_string(NULL_VALUE));
                emit("    cmp rax, rbx");
                emit("    je " + nullLbl);
                emit("    movq xmm0, rax");
                emitPrintDouble();
                emit("    jmp " + doneLbl);

                emit(stringLbl + ":");

                emit("    mov rbx, " + std::to_string(NULL_VALUE));
                emit("    cmp rax, rbx");
                emit("    je " + nullLbl);
                emit("    mov rsi, rax"); 
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
                emit("    jmp " + doneLbl);

                emit(boolLbl + ":");

                emit("    mov rbx, " + std::to_string(NULL_VALUE));
                emit("    cmp rax, rbx");
                emit("    je " + nullLbl);
                emitPrintBool();
                emit("    jmp " + doneLbl);

                emit(arrayLbl + ":");

                emit("    mov rbx, " + std::to_string(NULL_VALUE));
                emit("    cmp rax, rbx");
                emit("    je " + nullLbl);

                printNestedArray("rax", 0);
                emit("    jmp " + doneLbl);

                emit(nullLbl + ":");
                emitPrintNull();

                emit(doneLbl + ":");
                return;
            }

            std::string argType = getExprType(arg);
            genExpr(arg);
            emitPrintValue(argType);
        }

        void handleIndexExpr(const std::shared_ptr<IndexExpr> &idx)
        {
            genExpr(idx->array); 

            emitNullCheck("rax");

            emit("    push rax");     
            genExpr(idx->index);      
            emit("    mov rbx, rax"); 
            emit("    pop rax");      

            emitArrayBoundsCheck("rax", "rbx");

            emit("    push rax");                 
            emit("    push rbx");                 
            emit("    mov rcx, qword [rax + 8]"); 
            emit("    imul rbx, 8");              
            emit("    add rcx, rbx");
            emit("    mov rdx, qword [rcx]"); 
            emit("    pop rbx");              
            emit("    pop rax");              

            emit("    imul rbx, 8");
            emit("    add rbx, 16"); 
            emit("    add rax, rbx");
            emit("    mov rax, qword [rax]"); 

            emit("    push rdx"); 
        }

        void emit(const std::string &s) { out << s << "\n"; }
        std::unordered_map<std::string, std::string> varTypes; 

        std::string getExprType(const ExprPtr &expr)
        {
            if (auto lit = std::dynamic_pointer_cast<LiteralExpr>(expr))
            {
                return lit->type;
            }
            else if (auto id = std::dynamic_pointer_cast<IdentifierExpr>(expr))
            {
                auto it = varTypeInfo.find(id->name);
                if (it != varTypeInfo.end())
                {
                    return it->second->toString();
                }

                auto fallback = varTypes.find(id->name);
                if (fallback != varTypes.end())
                {
                    return fallback->second;
                }
                return "int";
            }

            else if (auto arr = std::dynamic_pointer_cast<ArrayExpr>(expr))
            {
                if (arr->elements.empty())
                    return "array<any>";

                std::string elemType = getExprType(arr->elements[0]);
                bool allSameType = true;

                for (size_t i = 1; i < arr->elements.size(); ++i)
                {
                    if (getExprType(arr->elements[i]) != elemType)
                    {
                        allSameType = false;
                        break;
                    }
                }

                return allSameType ? "array<" + elemType + ">" : "array<any>";
            }
            else if (auto idx = std::dynamic_pointer_cast<IndexExpr>(expr))
            {

                std::string arrayType = getExprType(idx->array);

                if (arrayType.find("array<") == 0 && arrayType.back() == '>')
                {

                    size_t start = 6;                    
                    size_t end = arrayType.length() - 1; 
                    return arrayType.substr(start, end - start);
                }

                if (auto arrayId = std::dynamic_pointer_cast<IdentifierExpr>(idx->array))
                {
                    auto typeIt = varTypeInfo.find(arrayId->name);
                    if (typeIt != varTypeInfo.end() && typeIt->second->elementType)
                    {
                        return typeIt->second->elementType->baseType;
                    }

                    auto elemIt = arrayElementTypes.find(arrayId->name);
                    if (elemIt != arrayElementTypes.end() && !elemIt->second.empty())
                    {

                        if (auto indexLit = std::dynamic_pointer_cast<LiteralExpr>(idx->index))
                        {
                            int indexVal = std::stoi(indexLit->value);
                            if (indexVal >= 0 && indexVal < static_cast<int>(elemIt->second.size()))
                            {
                                return elemIt->second[indexVal];
                            }
                        }

                        return elemIt->second[0];
                    }
                }

                return "int"; 
            }
            else if (auto bin = std::dynamic_pointer_cast<BinaryExpr>(expr))
            {
                if (bin->op == "=" || bin->op == "+=" || bin->op == "-=" ||
                    bin->op == "*=" || bin->op == "/=" || bin->op == "%=" ||
                    bin->op == "&=" || bin->op == "|=" || bin->op == "^=" ||
                    bin->op == "<<=" || bin->op == ">>=")
                {
                    return getExprType(bin->rhs);
                }
                else if (bin->op == "==" || bin->op == "!=" || bin->op == "<" ||
                         bin->op == "<=" || bin->op == ">" || bin->op == ">=" ||
                         bin->op == "&&" || bin->op == "||")
                {
                    return "bool";
                }
                else if (bin->op == "&" || bin->op == "|" || bin->op == "^" ||
                         bin->op == "<<" || bin->op == ">>" || bin->op == "%")
                {
                    return "int";
                }
                else
                {
                    std::string leftType = getExprType(bin->lhs);
                    std::string rightType = getExprType(bin->rhs);

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
                else if (un->op == "~")
                    return "int";
                else
                    return getExprType(un->rhs);
            }

            return "int";
        }

        void emitPrintNull()
        {
            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    lea rsi, [rel null_str]");
            emit("    mov rdx, 4");
            emit("    syscall");
        }

        void emitPrintValue(const std::string &varType, bool isNullable = false)
        {
            if (isNullable)
            {
                std::string notNullLbl = newLabel("not_null");
                std::string doneLbl = newLabel("print_done");

                emit("    mov rbx, " + std::to_string(NULL_VALUE));
                emit("    cmp rax, rbx");
                emit("    jne " + notNullLbl);

                emitPrintNull();
                emit("    jmp " + doneLbl);

                emit(notNullLbl + ":");

                if (varType == "float")
                {
                    emit("    movq xmm0, rax");
                    emitPrintDouble();
                }
                else if (varType == "bool")
                {
                    emitPrintBool();
                }
                else
                {
                    emitPrintRax();
                }

                emit(doneLbl + ":");
            }
            else
            {
                if (varType == "float")
                {
                    emit("    movq xmm0, rax");
                    emitPrintDouble();
                }
                else if (varType == "bool")
                {
                    emitPrintBool();
                }
                else
                {
                    emitPrintRax();
                }
            }
        }

        void emitPrintBool()
        {
            std::string lbl = newLabel("bool");

            emit("    cmp rax, 0");
            emit("    je " + lbl + "_false");

            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    lea rsi, [rel true_str]");
            emit("    mov rdx, 4");
            emit("    syscall");
            emit("    jmp " + lbl + "_done");

            emit(lbl + "_false:");

            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    lea rsi, [rel false_str]");
            emit("    mov rdx, 5");
            emit("    syscall");

            emit(lbl + "_done:");
        }

        void emitPrintDouble()
        {

            std::string baseLbl = newLabel("dbl");
            std::string positiveLbl = baseLbl + "_positive";
            std::string fracOkLbl = baseLbl + "_frac_ok";
            std::string fracPositiveLbl = baseLbl + "_frac_positive";
            std::string digitLoopLbl = baseLbl + "_digit_loop";
            std::string trimLoopLbl = baseLbl + "_trim_loop";
            std::string trimDoneLbl = baseLbl + "_trim_done";

            emit("    push rbx");
            emit("    push r12");
            emit("    push r13");
            emit("    push r14");
            emit("    sub rsp, 16");
            emit("    movsd [rsp], xmm0"); 

            emit("    xorpd xmm2, xmm2"); 
            emit("    comisd xmm0, xmm2");
            emit("    jae " + positiveLbl);

            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    lea rsi, [rel minus_char]");
            emit("    mov rdx, 1");
            emit("    syscall");
            emit("    movsd xmm0, [rsp]");
            emit("    mov rax, 0x8000000000000000"); 
            emit("    movq xmm1, rax");
            emit("    xorpd xmm0, xmm1"); 
            emit("    movsd [rsp], xmm0");

            emit(positiveLbl + ":");

            emit("    movsd xmm0, [rsp]");
            emit("    cvttsd2si rax, xmm0");
            emit("    mov [rsp+8], rax"); 
            emitPrintRax();

            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    lea rsi, [rel dot_char]");
            emit("    mov rdx, 1");
            emit("    syscall");

            emit("    movsd xmm0, [rsp]");  
            emit("    mov rax, [rsp+8]");   
            emit("    cvtsi2sd xmm1, rax"); 
            emit("    subsd xmm0, xmm1");   

            emit("    mov rax, 1000000");
            emit("    cvtsi2sd xmm1, rax");
            emit("    mulsd xmm0, xmm1");
            emit("    addsd xmm0, [rel half_const]"); 
            emit("    cvttsd2si rax, xmm0");

            emit("    cmp rax, 1000000");
            emit("    jl " + fracOkLbl);
            emit("    mov rax, 999999");
            emit(fracOkLbl + ":");

            emit("    cmp rax, 0");
            emit("    jge " + fracPositiveLbl);
            emit("    mov rax, 0");
            emit(fracPositiveLbl + ":");

            emit("    mov rbx, rax");           
            emit("    lea r14, [rel num_buf]"); 
            emit("    mov r13, r14");           
            emit("    add r13, 6");             
            emit("    mov byte [r13], 0");      

            emit("    mov rcx, 6"); 

            emit(digitLoopLbl + ":");
            emit("    xor rdx, rdx");
            emit("    mov rax, rbx");
            emit("    mov r8, 10");
            emit("    div r8");      
            emit("    add dl, '0'"); 
            emit("    dec r13");
            emit("    mov [r13], dl"); 
            emit("    mov rbx, rax");  
            emit("    loop " + digitLoopLbl);

            emit("    mov rsi, r14"); 
            emit("    add rsi, 5");   
            emit("    mov rcx, 6");   

            emit(trimLoopLbl + ":");
            emit("    cmp rcx, 1"); 
            emit("    jle " + trimDoneLbl);
            emit("    cmp byte [rsi], '0'");
            emit("    jne " + trimDoneLbl);
            emit("    dec rcx");
            emit("    dec rsi");
            emit("    jmp " + trimLoopLbl);

            emit(trimDoneLbl + ":");

            emit("    mov rax, 1");   
            emit("    mov rdi, 1");   
            emit("    mov rsi, r14"); 
            emit("    mov rdx, rcx"); 
            emit("    syscall");

            emit("    add rsp, 16");
            emit("    pop r14");
            emit("    pop r13");
            emit("    pop r12");
            emit("    pop rbx");
        }

        void emitPrintRax()
        {
            std::string baseLbl = newLabel("conv");
            std::string positiveLbl = baseLbl + "_positive";
            std::string loopLbl = baseLbl + "_loop";

            emit("    cmp rax, 0");
            emit("    jge " + positiveLbl);

            emit("    push rax");
            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    lea rsi, [rel minus_char]");
            emit("    mov rdx, 1");
            emit("    syscall");
            emit("    pop rax");
            emit("    neg rax");

            emit(positiveLbl + ":");

            emit("    push rbx");
            emit("    push r12");
            emit("    push r13");

            emit("    mov rbx, 10");
            emit("    lea r12, [rel num_buf+31]"); 
            emit("    mov byte [r12], 0");         
            emit("    mov r13, r12");

            emit(loopLbl + ":");
            emit("    xor rdx, rdx");
            emit("    div rbx"); 
            emit("    add dl, '0'");
            emit("    dec r13");
            emit("    mov [r13], dl");
            emit("    test rax, rax");
            emit("    jnz " + loopLbl);

            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    mov rsi, r13");
            emit("    lea rdx, [rel num_buf+31]");
            emit("    sub rdx, r13"); 
            emit("    syscall");

            emit("    pop r13");
            emit("    pop r12");
            emit("    pop rbx");
        }

        void handleArrayExpr(const std::shared_ptr<ArrayExpr> &arr)
        {
            int size = arr->elements.size();

            if (size == 0)
            {

                emit("    mov rax, " + std::to_string(NULL_VALUE));
                return;
            }

            int totalBytes = 8 + 8 + (size * 8); 

            emit("    mov rax, 9");                             
            emit("    mov rdi, 0");                             
            emit("    mov rsi, " + std::to_string(totalBytes)); 
            emit("    mov rdx, 3");                             
            emit("    mov r10, 34");                            
            emit("    mov r8, -1");                             
            emit("    mov r9, 0");                              
            emit("    syscall");

            emit("    mov qword [rax], " + std::to_string(size));
            emit("    push rax"); 

            int typeBytes = size * 8; 
            emit("    mov rax, 9");   
            emit("    mov rdi, 0");
            emit("    mov rsi, " + std::to_string(typeBytes));
            emit("    mov rdx, 3");
            emit("    mov r10, 34");
            emit("    mov r8, -1");
            emit("    mov r9, 0");
            emit("    syscall");

            emit("    mov rbx, [rsp]");           
            emit("    mov qword [rbx + 8], rax"); 

            emit("    push rax"); 

            for (int i = 0; i < size; ++i)
            {
                std::string elemType = getExprType(arr->elements[i]);

                emit("    mov rbx, [rsp]"); 
                int typeCode = 0;           
                if (elemType == "float")
                    typeCode = 1;
                else if (elemType == "string")
                    typeCode = 2;
                else if (elemType == "bool")
                    typeCode = 3;
                else if (elemType.find("array") == 0)
                    typeCode = 4; 

                emit("    mov qword [rbx + " + std::to_string(i * 8) + "], " + std::to_string(typeCode));

                genExpr(arr->elements[i]);

                emit("    mov rbx, [rsp + 8]"); 
                emit("    mov qword [rbx + " + std::to_string(16 + i * 8) + "], rax");
            }

            emit("    add rsp, 8"); 
            emit("    pop rax");    
        }

        void printArrayVariable(const std::string &varName)
        {
            int off = varTable[varName];
            emit("    mov rax, " + slot(off)); 

            emitNullCheck("rax");

            emit("    mov rbx, qword [rax]");     
            emit("    mov rcx, qword [rax + 8]"); 
            emit("    add rax, 16");              

            emit("    push rax");
            emit("    push rbx");
            emit("    push rcx");
            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    mov rsi, open_bracket");
            emit("    mov rdx, 1");
            emit("    syscall");
            emit("    pop rcx");
            emit("    pop rbx");
            emit("    pop rax");

            std::string loopLbl = newLabel("print_loop");
            std::string doneLbl = newLabel("print_done");
            std::string commaLbl = newLabel("print_comma");

            emit("    mov r8, 0"); 

            emit(loopLbl + ":");
            emit("    cmp r8, rbx");
            emit("    jge " + doneLbl);

            emit("    cmp r8, 0");
            emit("    je " + commaLbl);

            emit("    push rax");
            emit("    push rbx");
            emit("    push rcx");
            emit("    push r8");
            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    mov rsi, comma_space");
            emit("    mov rdx, 2");
            emit("    syscall");
            emit("    pop r8");
            emit("    pop rcx");
            emit("    pop rbx");
            emit("    pop rax");

            emit(commaLbl + ":");

            emit("    push rax");
            emit("    push rbx");
            emit("    mov rbx, r8");
            emit("    imul rbx, 8");
            emit("    add rcx, rbx");
            emit("    mov rdx, qword [rcx]"); 
            emit("    sub rcx, rbx");         
            emit("    pop rbx");
            emit("    pop rax");

            emit("    push rcx");
            emit("    push rdx");
            emit("    mov rcx, r8");
            emit("    imul rcx, 8");
            emit("    add rax, rcx");
            emit("    mov r9, qword [rax]"); 
            emit("    sub rax, rcx");        
            emit("    pop rdx");
            emit("    pop rcx");

            std::string intLbl = newLabel("print_int");
            std::string floatLbl = newLabel("print_float");
            std::string stringLbl = newLabel("print_string");
            std::string boolLbl = newLabel("print_bool");
            std::string arrayLbl = newLabel("print_array"); 
            std::string nullLbl = newLabel("print_null");   
            std::string nextLbl = newLabel("next_elem");

            emit("    cmp rdx, 0");
            emit("    je " + intLbl);
            emit("    cmp rdx, 1");
            emit("    je " + floatLbl);
            emit("    cmp rdx, 2");
            emit("    je " + stringLbl);
            emit("    cmp rdx, 3");
            emit("    je " + boolLbl);
            emit("    cmp rdx, 4");
            emit("    je " + arrayLbl); 
            emit("    jmp " + intLbl);  

            emit(intLbl + ":");
            emit("    push rax");
            emit("    push rbx");
            emit("    push rcx");
            emit("    push r8");

            emit("    mov rbx, " + std::to_string(NULL_VALUE));
            emit("    cmp r9, rbx");
            emit("    je " + nullLbl);

            emit("    mov rax, r9");
            emitPrintRax();
            emit("    pop r8");
            emit("    pop rcx");
            emit("    pop rbx");
            emit("    pop rax");
            emit("    jmp " + nextLbl);

            emit(floatLbl + ":");
            emit("    push rax");
            emit("    push rbx");
            emit("    push rcx");
            emit("    push r8");

            emit("    mov rbx, " + std::to_string(NULL_VALUE));
            emit("    cmp r9, rbx");
            emit("    je " + nullLbl);

            emit("    movq xmm0, r9");
            emitPrintDouble();
            emit("    pop r8");
            emit("    pop rcx");
            emit("    pop rbx");
            emit("    pop rax");
            emit("    jmp " + nextLbl);

            emit(stringLbl + ":");
            emit("    push rax");
            emit("    push rbx");
            emit("    push rcx");
            emit("    push r8");

            emit("    mov rbx, " + std::to_string(NULL_VALUE));
            emit("    cmp r9, rbx");
            emit("    je " + nullLbl);

            emit("    mov rsi, r9"); 
            std::string strlenLbl = newLabel("strlen");
            emit("    xor rcx, rcx");
            emit(strlenLbl + "_loop:");
            emit("    cmp byte [rsi+rcx], 0");
            emit("    je " + strlenLbl + "_done");
            emit("    inc rcx");
            emit("    jmp " + strlenLbl + "_loop");
            emit(strlenLbl + "_done:");
            emit("    mov rdx, rcx");
            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    syscall");
            emit("    pop r8");
            emit("    pop rcx");
            emit("    pop rbx");
            emit("    pop rax");
            emit("    jmp " + nextLbl);

            emit(boolLbl + ":");
            emit("    push rax");
            emit("    push rbx");
            emit("    push rcx");
            emit("    push r8");

            emit("    mov rbx, " + std::to_string(NULL_VALUE));
            emit("    cmp r9, rbx");
            emit("    je " + nullLbl);

            emit("    mov rax, r9");
            emitPrintBool();
            emit("    pop r8");
            emit("    pop rcx");
            emit("    pop rbx");
            emit("    pop rax");
            emit("    jmp " + nextLbl);

            emit(arrayLbl + ":");
            emit("    push rax");
            emit("    push rbx");
            emit("    push rcx");
            emit("    push r8");

            emit("    mov rbx, " + std::to_string(NULL_VALUE));
            emit("    cmp r9, rbx");
            emit("    je " + nullLbl);

            printNestedArray("r9", 0);
            emit("    pop r8");
            emit("    pop rcx");
            emit("    pop rbx");
            emit("    pop rax");
            emit("    jmp " + nextLbl);

            emit(nullLbl + ":");
            emit("    push rax");
            emit("    push rbx");
            emit("    push rcx");
            emit("    push r8");
            emitPrintNull();
            emit("    pop r8");
            emit("    pop rcx");
            emit("    pop rbx");
            emit("    pop rax");

            emit(nextLbl + ":");
            emit("    inc r8");
            emit("    jmp " + loopLbl);

            emit(doneLbl + ":");

            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    mov rsi, close_bracket");
            emit("    mov rdx, 1");
            emit("    syscall");
        }

        void printNestedArray(const std::string &arrayReg, int depth = 0)
        {

            if (depth > 10)
            {
                emit("    mov rax, 1");
                emit("    mov rdi, 1");
                emit("    lea rsi, [rel null_str]"); 
                emit("    mov rdx, 4");
                emit("    syscall");
                return;
            }

            emit("    mov rax, " + arrayReg);     
            emit("    mov rbx, qword [rax]");     
            emit("    mov rcx, qword [rax + 8]"); 
            emit("    add rax, 16");              

            emit("    push rax");
            emit("    push rbx");
            emit("    push rcx");
            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    mov rsi, open_bracket");
            emit("    mov rdx, 1");
            emit("    syscall");
            emit("    pop rcx");
            emit("    pop rbx");
            emit("    pop rax");

            std::string loopLbl = newLabel("nested_loop");
            std::string doneLbl = newLabel("nested_done");
            std::string commaLbl = newLabel("nested_comma");

            emit("    mov r8, 0"); 

            emit(loopLbl + ":");
            emit("    cmp r8, rbx");
            emit("    jge " + doneLbl);

            emit("    cmp r8, 0");
            emit("    je " + commaLbl);

            emit("    push rax");
            emit("    push rbx");
            emit("    push rcx");
            emit("    push r8");
            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    mov rsi, comma_space");
            emit("    mov rdx, 2");
            emit("    syscall");
            emit("    pop r8");
            emit("    pop rcx");
            emit("    pop rbx");
            emit("    pop rax");

            emit(commaLbl + ":");

            emit("    push rax");
            emit("    push rbx");
            emit("    mov rbx, r8");
            emit("    imul rbx, 8");
            emit("    add rcx, rbx");
            emit("    mov rdx, qword [rcx]"); 
            emit("    sub rcx, rbx");
            emit("    pop rbx");
            emit("    pop rax");

            emit("    push rcx");
            emit("    push rdx");
            emit("    mov rcx, r8");
            emit("    imul rcx, 8");
            emit("    add rax, rcx");
            emit("    mov r9, qword [rax]"); 
            emit("    sub rax, rcx");
            emit("    pop rdx");
            emit("    pop rcx");

            std::string intLbl = newLabel("nested_int");
            std::string floatLbl = newLabel("nested_float");
            std::string stringLbl = newLabel("nested_string");
            std::string boolLbl = newLabel("nested_bool");
            std::string arrayLbl = newLabel("nested_array");
            std::string nullLbl = newLabel("nested_null");
            std::string nextLbl = newLabel("nested_next");

            emit("    cmp rdx, 0");
            emit("    je " + intLbl);
            emit("    cmp rdx, 1");
            emit("    je " + floatLbl);
            emit("    cmp rdx, 2");
            emit("    je " + stringLbl);
            emit("    cmp rdx, 3");
            emit("    je " + boolLbl);
            emit("    cmp rdx, 4");
            emit("    je " + arrayLbl);
            emit("    jmp " + intLbl); 

            emit(intLbl + ":");
            emit("    push rax");
            emit("    push rbx");
            emit("    push rcx");
            emit("    push r8");

            emit("    mov rbx, " + std::to_string(NULL_VALUE));
            emit("    cmp r9, rbx");
            emit("    je " + nullLbl);

            emit("    mov rax, r9");
            emitPrintRax();
            emit("    pop r8");
            emit("    pop rcx");
            emit("    pop rbx");
            emit("    pop rax");
            emit("    jmp " + nextLbl);

            emit(floatLbl + ":");
            emit("    push rax");
            emit("    push rbx");
            emit("    push rcx");
            emit("    push r8");

            emit("    mov rbx, " + std::to_string(NULL_VALUE));
            emit("    cmp r9, rbx");
            emit("    je " + nullLbl);

            emit("    movq xmm0, r9");
            emitPrintDouble();
            emit("    pop r8");
            emit("    pop rcx");
            emit("    pop rbx");
            emit("    pop rax");
            emit("    jmp " + nextLbl);

            emit(stringLbl + ":");
            emit("    push rax");
            emit("    push rbx");
            emit("    push rcx");
            emit("    push r8");

            emit("    mov rbx, " + std::to_string(NULL_VALUE));
            emit("    cmp r9, rbx");
            emit("    je " + nullLbl);

            emit("    mov rsi, r9");
            std::string strlenLbl = newLabel("nested_strlen");
            emit("    xor rcx, rcx");
            emit(strlenLbl + "_loop:");
            emit("    cmp byte [rsi+rcx], 0");
            emit("    je " + strlenLbl + "_done");
            emit("    inc rcx");
            emit("    jmp " + strlenLbl + "_loop");
            emit(strlenLbl + "_done:");
            emit("    mov rdx, rcx");
            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    syscall");
            emit("    pop r8");
            emit("    pop rcx");
            emit("    pop rbx");
            emit("    pop rax");
            emit("    jmp " + nextLbl);

            emit(boolLbl + ":");
            emit("    push rax");
            emit("    push rbx");
            emit("    push rcx");
            emit("    push r8");

            emit("    mov rbx, " + std::to_string(NULL_VALUE));
            emit("    cmp r9, rbx");
            emit("    je " + nullLbl);

            emit("    mov rax, r9");
            emitPrintBool();
            emit("    pop r8");
            emit("    pop rcx");
            emit("    pop rbx");
            emit("    pop rax");
            emit("    jmp " + nextLbl);

            emit(arrayLbl + ":");
            emit("    push rax");
            emit("    push rbx");
            emit("    push rcx");
            emit("    push r8");

            emit("    mov rbx, " + std::to_string(NULL_VALUE));
            emit("    cmp r9, rbx");
            emit("    je " + nullLbl);

            printNestedArray("r9", depth + 1);
            emit("    pop r8");
            emit("    pop rcx");
            emit("    pop rbx");
            emit("    pop rax");
            emit("    jmp " + nextLbl);

            emit(nullLbl + ":");
            emit("    push rax");
            emit("    push rbx");
            emit("    push rcx");
            emit("    push r8");
            emitPrintNull();
            emit("    pop r8");
            emit("    pop rcx");
            emit("    pop rbx");
            emit("    pop rax");

            emit(nextLbl + ":");
            emit("    inc r8");
            emit("    jmp " + loopLbl);

            emit(doneLbl + ":");

            emit("    mov rax, 1");
            emit("    mov rdi, 1");
            emit("    mov rsi, close_bracket");
            emit("    mov rdx, 1");
            emit("    syscall");
        }

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

                bool isNullable = v->typeHint.empty() || v->typeHint.back() == '?';
                varNullable[v->name] = isNullable;

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
                    if (isNullable)
                    {

                        emit("    mov rax, " + std::to_string(NULL_VALUE));
                        emit("    mov " + slot(off) + ", rax");
                    }
                    else
                    {

                        emit("    mov rax, 0");
                        emit("    mov " + slot(off) + ", rax");
                    }
                }
            }

            else if (auto f = std::dynamic_pointer_cast<FuncDecl>(stmt))
            {
                if (!isTopLevel)
                {

                    deferredFunctions.push_back(f);

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
            else if (auto forStmt = std::dynamic_pointer_cast<ForStmt>(stmt))
            {

                collectLocals(forStmt->body, isTopLevel);
            }
        }
        void generate(StmtPtr program)
        {

            deferredFunctions.clear();

            emit("section .text");
            emit("global _start");
            emit("_start:");
            emit("    call main");

            emit("    mov rdi, rax"); 
            emit("    mov rax, 60");  
            emit("    syscall");

            genStmt(program);

            emitErrorHandlers();

            emit("section .bss");
            emit("num_buf: resb 32");

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

                std::unordered_map<std::string, int> savedVarTable = varTable;
                std::unordered_map<std::string, std::string> savedVarTypes = varTypes;
                int savedStackOffset = stackOffset;
                auto savedDeferred = deferredFunctions;

                varTable.clear();
                varTypes.clear();
                stackOffset = 0;
                deferredFunctions.clear();

                int paramOffset = 16; 

                for (const auto &param : f->params)
                {
                    const std::string &paramName = param.first;
                    const std::string &paramType = param.second.empty() ? "any" : param.second;

                    varTable[paramName] = paramOffset;
                    varTypes[paramName] = paramType;
                    paramOffset += 8;
                }

                collectLocals(f->body, false);

                emit(f->name + ":");
                emit("    push rbp");
                emit("    mov rbp, rsp");

                int totalLocal = -stackOffset;
                if (totalLocal % 16 != 0)
                    totalLocal += 8;

                if (totalLocal > 0)
                    emit("    sub rsp, " + std::to_string(totalLocal));

                genStmtSkipNestedFunctions(f->body);

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

                emit("    mov rsp, rbp");
                emit("    pop rbp");
                emit("    ret");

                for (auto &nestedFunc : deferredFunctions)
                {
                    genStmt(nestedFunc);
                }

                varTable = savedVarTable;
                varTypes = savedVarTypes;
                stackOffset = savedStackOffset;
                deferredFunctions = savedDeferred;
            }
            if (auto v = std::dynamic_pointer_cast<VarDecl>(stmt))
            {
                if (varTable.find(v->name) == varTable.end())
                {
                    stackOffset -= 8;
                    varTable[v->name] = stackOffset;
                }

                if (v->typeHint.find('[') != std::string::npos)
                {

                    auto typeInfo = TypeInfo::parse(v->typeHint);
                    varTypeInfo[v->name] = typeInfo;

                    if (!typeInfo->dimensions.empty())
                    {
                        arrayDimensions[v->name] = typeInfo->dimensions[0];
                    }
                }

                bool isNullable = v->typeHint.empty() || v->typeHint.back() == '?';
                varNullable[v->name] = isNullable;

                if (v->initializer)
                {
                    if (auto litInit = std::dynamic_pointer_cast<LiteralExpr>(v->initializer))
                    {
                        if (litInit->type == "string")
                            varTypes[v->name] = "string";
                        else if (litInit->type == "int")
                            varTypes[v->name] = "int";
                        else if (litInit->type == "float")
                            varTypes[v->name] = "float";
                        else if (litInit->type == "null")
                            varTypes[v->name] = "null";
                    }
                    else if (auto callInit = std::dynamic_pointer_cast<CallExpr>(v->initializer))
                    {
                        std::string returnType = getExprType(callInit);
                        varTypes[v->name] = returnType;
                    }
                    else
                    {
                        std::string inferredType = getExprType(v->initializer);
                        varTypes[v->name] = inferredType;
                    }

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

                if (auto arrInit = std::dynamic_pointer_cast<ArrayExpr>(v->initializer))
                {
                    for (int i = 0; i < static_cast<int>(arrInit->elements.size()); ++i)
                    {
                        std::string elemType = getExprType(arrInit->elements[i]);
                        storeArrayElement(v->name, i, elemType);
                    }
                }
                else
                {
                    int off = varTable[v->name];
                    if (isNullable)
                    {

                        emit("    mov rax, " + std::to_string(NULL_VALUE));
                        emit("    mov " + slot(off) + ", rax");
                    }
                    else
                    {

                        emit("    mov rax, 0");
                        emit("    mov " + slot(off) + ", rax");
                    }
                }
            }

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
                                else if (lit->type == "null")
                                {
                                    emitPrintNull();
                                    return;
                                }
                            }
                            else if (auto idArg = std::dynamic_pointer_cast<IdentifierExpr>(arg))
                            {

                                std::string argType = getExprType(arg);
                                if (argType.find("array") == 0)
                                {

                                    printArrayVariable(idArg->name);
                                    return;
                                }
                                else if (auto it = varTypes.find(idArg->name); it != varTypes.end() && it->second == "string")
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

                            handleArrayElementPrint(arg);
                            return;
                        }
                        else
                        {
                            genExpr(e->expr);
                            return;
                        }
                    }
                }

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
                std::string elseLbl = newLabel("Lelse");
                std::string endLbl = newLabel("Lend");

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
                std::string startLbl = newLabel("Lwhile_start");
                std::string endLbl = newLabel("Lwhile_end");

                emit(startLbl + ":");

                genExpr(w->condition);

                emit("    cmp rax, 0");
                emit("    je " + endLbl);

                genStmt(w->body);

                emit("    jmp " + startLbl);

                emit(endLbl + ":");
            }

            else if (auto t = std::dynamic_pointer_cast<TimesStmt>(stmt))
            {
                std::string startLbl = newLabel("Ltimes_start");
                std::string endLbl = newLabel("Ltimes_end");

                std::string countType = getExprType(t->count);
                if (countType == "float")
                {
                    genExpr(t->count);               
                    emit("    cvttsd2si rax, xmm0"); 
                }
                else
                {
                    genExpr(t->count); 
                }

                emit("    sub rsp, 16");
                emit("    mov qword [rsp], rax"); 

                emit(startLbl + ":");
                emit("    mov rax, qword [rsp]");
                emit("    cmp rax, 0");
                emit("    jle " + endLbl); 

                genStmt(t->body);

                emit("    dec qword [rsp]");
                emit("    jmp " + startLbl);

                emit(endLbl + ":");
                emit("    add rsp, 16"); 
            }

            else if (auto forStmt = std::dynamic_pointer_cast<ForStmt>(stmt))
            {
                std::string startLbl = newLabel("for_start");
                std::string endLbl = newLabel("for_end");

                genExpr(forStmt->iterable);

                emitNullCheck("rax");

                emit("    sub rsp, 16");          
                emit("    mov [rsp], rax");       
                emit("    mov rbx, qword [rax]"); 
                emit("    mov [rsp+8], rbx");     

                stackOffset -= 8;
                int iteratorOffset = stackOffset;
                varTable[forStmt->iterator] = iteratorOffset;
                varTypes[forStmt->iterator] = "int"; 

                emit("    mov qword " + slot(iteratorOffset) + ", 0");

                emit(startLbl + ":");

                emit("    mov rax, [rsp]");                   
                emit("    mov rbx, [rsp+8]");                 
                emit("    mov rcx, " + slot(iteratorOffset)); 

                emit("    cmp rcx, rbx");
                emit("    jge " + endLbl);

                genStmt(forStmt->body);

                emit("    inc qword " + slot(iteratorOffset));
                emit("    jmp " + startLbl);

                emit(endLbl + ":");

                emit("    add rsp, 16");           
                stackOffset += 8;                  
                varTable.erase(forStmt->iterator); 
                varTypes.erase(forStmt->iterator);
            }
            else if (auto p = std::dynamic_pointer_cast<PrintStmt>(stmt))
            {

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
                        genExpr(p->expr); 
                        emitPrintRax();   
                    }
                }
                else
                {
                    genExpr(p->expr); 
                    emitPrintRax();   
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
                else if (lit->type == "null")
                    emit("    mov rax, " + std::to_string(NULL_VALUE));
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
                    emit("    movsd xmm0, [rel " + lbl + "]");
                    emit("    movq rax, xmm0");
                }
            }
            else if (auto id = std::dynamic_pointer_cast<IdentifierExpr>(expr))
            {
                if (varTable.find(id->name) == varTable.end())
                    throw std::runtime_error("Unknown variable: " + id->name);
                int off = varTable[id->name];

                auto it = varTypes.find(id->name);
                if (it != varTypes.end() && it->second == "float")
                {
                    emit("    movsd xmm0, " + slot(off));
                    emit("    movq rax, xmm0"); 
                }
                else
                {
                    emit("    mov rax, " + slot(off));
                }
            }

            else if (auto bin = std::dynamic_pointer_cast<BinaryExpr>(expr))
            {

                if (bin->op == "=" || bin->op == "+=" || bin->op == "-=" ||
                    bin->op == "*=" || bin->op == "/=" || bin->op == "%=" ||
                    bin->op == "&=" || bin->op == "|=" || bin->op == "^=" ||
                    bin->op == "<<=" || bin->op == ">>=")
                {
                    auto id = std::dynamic_pointer_cast<IdentifierExpr>(bin->lhs);
                    if (!id)
                        throw std::runtime_error("Invalid assignment target");

                    int off = varTable[id->name];

                    if (bin->op == "=")
                    {

                        if (auto rhsLit = std::dynamic_pointer_cast<LiteralExpr>(bin->rhs))
                        {
                            if (rhsLit->type == "null")
                            {
                                auto nullIt = varNullable.find(id->name);
                                if (nullIt != varNullable.end() && !nullIt->second)
                                {
                                    throw std::runtime_error("Cannot assign null to non-nullable variable: " + id->name);
                                }
                            }
                        }

                        genExpr(bin->rhs);
                        auto it = varTypes.find(id->name);
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

                        genExpr(bin->lhs); 
                        emit("    push rax");
                        genExpr(bin->rhs); 
                        emit("    mov rbx, rax");
                        emit("    pop rax");

                        if (bin->op == "+=")
                            emit("    add rax, rbx");
                        else if (bin->op == "-=")
                            emit("    sub rax, rbx");
                        else if (bin->op == "*=")
                            emit("    imul rax, rbx");
                        else if (bin->op == "/=")
                        {
                            emit("    cqo");
                            emit("    idiv rbx");
                        }
                        else if (bin->op == "%=")
                        {
                            emit("    cqo");
                            emit("    idiv rbx");
                            emit("    mov rax, rdx"); 
                        }
                        else if (bin->op == "&=")
                            emit("    and rax, rbx");
                        else if (bin->op == "|=")
                            emit("    or rax, rbx");
                        else if (bin->op == "^=")
                            emit("    xor rax, rbx");
                        else if (bin->op == "<<=")
                        {
                            emit("    mov rcx, rbx");
                            emit("    shl rax, cl");
                        }
                        else if (bin->op == ">>=")
                        {
                            emit("    mov rcx, rbx");
                            emit("    sar rax, cl"); 
                        }

                        emit("    mov " + slot(off) + ", rax");
                    }
                }

                else if (bin->op == "&&")
                {
                    std::string falseLbl = newLabel("and_false");
                    std::string doneLbl = newLabel("and_done");

                    genExpr(bin->lhs);
                    emit("    cmp rax, 0");
                    emit("    je " + falseLbl);

                    genExpr(bin->rhs);
                    emit("    cmp rax, 0");
                    emit("    setne al");
                    emit("    movzx rax, al");
                    emit("    jmp " + doneLbl);

                    emit(falseLbl + ":");
                    emit("    mov rax, 0");
                    emit(doneLbl + ":");
                }
                else if (bin->op == "||")
                {
                    std::string trueLbl = newLabel("or_true");
                    std::string doneLbl = newLabel("or_done");

                    genExpr(bin->lhs);
                    emit("    cmp rax, 0");
                    emit("    jne " + trueLbl);

                    genExpr(bin->rhs);
                    emit("    cmp rax, 0");
                    emit("    setne al");
                    emit("    movzx rax, al");
                    emit("    jmp " + doneLbl);

                    emit(trueLbl + ":");
                    emit("    mov rax, 1");
                    emit(doneLbl + ":");
                }

                else if (bin->op == "==" || bin->op == "!=")
                {

                    bool leftIsFloat = false, rightIsFloat = false;

                    if (auto leftId = std::dynamic_pointer_cast<IdentifierExpr>(bin->lhs))
                    {
                        auto it = varTypes.find(leftId->name);
                        leftIsFloat = (it != varTypes.end() && it->second == "float");
                    }
                    else if (auto leftLit = std::dynamic_pointer_cast<LiteralExpr>(bin->lhs))
                    {
                        leftIsFloat = (leftLit->type == "float");
                    }

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

                        genExpr(bin->lhs);
                        if (!leftIsFloat)
                        {
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
                            emit("    cvtsi2sd xmm1, rax");
                        }
                        else
                        {
                            emit("    movq xmm1, rax");
                        }

                        emit("    movsd xmm0, [rsp]");
                        emit("    add rsp, 8");

                        emit("    comisd xmm0, xmm1");
                        if (bin->op == "==")
                        {
                            emit("    sete al");
                            emit("    movzx rax, al");
                        }
                        else 
                        {
                            emit("    setne al");
                            emit("    movzx rax, al");
                        }
                    }

                    else if (isStringComparison(bin->lhs, bin->rhs))
                    {

                        genExpr(bin->lhs); 
                        emit("    push rax");
                        genExpr(bin->rhs);        
                        emit("    mov rsi, rax"); 
                        emit("    pop rdi");      

                        std::string cmpLbl = newLabel("strcmp");
                        std::string eqLbl = cmpLbl + "_eq";
                        std::string neLbl = cmpLbl + "_ne";
                        std::string doneLbl = cmpLbl + "_done";

                        emit(cmpLbl + "_loop:");
                        emit("    mov al, [rdi]");
                        emit("    mov bl, [rsi]");
                        emit("    cmp al, bl");
                        emit("    jne " + neLbl);
                        emit("    test al, al"); 
                        emit("    je " + eqLbl);
                        emit("    inc rdi");
                        emit("    inc rsi");
                        emit("    jmp " + cmpLbl + "_loop");

                        emit(eqLbl + ":");
                        if (bin->op == "==")
                            emit("    mov rax, 1"); 
                        else
                            emit("    mov rax, 0"); 
                        emit("    jmp " + doneLbl);

                        emit(neLbl + ":");
                        if (bin->op == "==")
                            emit("    mov rax, 0"); 
                        else
                            emit("    mov rax, 1"); 

                        emit(doneLbl + ":");
                    }
                    else
                    {

                        genExpr(bin->lhs);
                        emit("    push rax");
                        genExpr(bin->rhs);
                        emit("    mov rbx, rax");
                        emit("    pop rax");

                        emit("    cmp rax, rbx");
                        if (bin->op == "==")
                        {
                            emit("    sete al");
                            emit("    movzx rax, al");
                        }
                        else 
                        {
                            emit("    setne al");
                            emit("    movzx rax, al");
                        }
                    }
                }
                else if (bin->op == "<" || bin->op == "<=" || bin->op == ">" || bin->op == ">=")
                {

                    bool leftIsFloat = false, rightIsFloat = false;

                    if (auto leftId = std::dynamic_pointer_cast<IdentifierExpr>(bin->lhs))
                    {
                        auto it = varTypes.find(leftId->name);
                        leftIsFloat = (it != varTypes.end() && it->second == "float");
                    }
                    else if (auto leftLit = std::dynamic_pointer_cast<LiteralExpr>(bin->lhs))
                    {
                        leftIsFloat = (leftLit->type == "float");
                    }

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

                        genExpr(bin->lhs);
                        if (!leftIsFloat)
                        {
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
                            emit("    cvtsi2sd xmm1, rax");
                        }
                        else
                        {
                            emit("    movq xmm1, rax");
                        }

                        emit("    movsd xmm0, [rsp]");
                        emit("    add rsp, 8");

                        emit("    comisd xmm0, xmm1");
                        if (bin->op == "<")
                        {
                            emit("    setb al");
                            emit("    movzx rax, al");
                        }
                        else if (bin->op == "<=")
                        {
                            emit("    setbe al");
                            emit("    movzx rax, al");
                        }
                        else if (bin->op == ">")
                        {
                            emit("    seta al");
                            emit("    movzx rax, al");
                        }
                        else 
                        {
                            emit("    setae al");
                            emit("    movzx rax, al");
                        }
                    }
                    else
                    {

                        genExpr(bin->lhs);
                        emit("    push rax");
                        genExpr(bin->rhs);
                        emit("    mov rbx, rax");
                        emit("    pop rax");

                        emit("    cmp rax, rbx");
                        if (bin->op == "<")
                        {
                            emit("    setl al");
                            emit("    movzx rax, al");
                        }
                        else if (bin->op == "<=")
                        {
                            emit("    setle al");
                            emit("    movzx rax, al");
                        }
                        else if (bin->op == ">")
                        {
                            emit("    setg al");
                            emit("    movzx rax, al");
                        }
                        else 
                        {
                            emit("    setge al");
                            emit("    movzx rax, al");
                        }
                    }
                }

                else
                {

                    bool leftIsFloat = false, rightIsFloat = false;

                    if (auto leftId = std::dynamic_pointer_cast<IdentifierExpr>(bin->lhs))
                    {
                        auto it = varTypes.find(leftId->name);
                        leftIsFloat = (it != varTypes.end() && it->second == "float");
                    }
                    else if (auto leftLit = std::dynamic_pointer_cast<LiteralExpr>(bin->lhs))
                    {
                        leftIsFloat = (leftLit->type == "float");
                    }
                    else if (auto leftIdx = std::dynamic_pointer_cast<IndexExpr>(bin->lhs))
                    {

                        std::string idxType = getExprType(leftIdx);
                        leftIsFloat = (idxType == "float");
                    }

                    if (auto rightId = std::dynamic_pointer_cast<IdentifierExpr>(bin->rhs))
                    {
                        auto it = varTypes.find(rightId->name);
                        rightIsFloat = (it != varTypes.end() && it->second == "float");
                    }
                    else if (auto rightLit = std::dynamic_pointer_cast<LiteralExpr>(bin->rhs))
                    {
                        rightIsFloat = (rightLit->type == "float");
                    }
                    else if (auto rightIdx = std::dynamic_pointer_cast<IndexExpr>(bin->rhs))
                    {

                        std::string idxType = getExprType(rightIdx);
                        rightIsFloat = (idxType == "float");
                    }

                    if (leftIsFloat || rightIsFloat)
                    {

                        genExpr(bin->lhs);
                        if (!leftIsFloat)
                        {

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
                        else if (bin->op == "%") 
                        {
                            emit("    cqo");
                            emit("    idiv rbx");
                            emit("    mov rax, rdx"); 
                        }

                        else if (bin->op == "&")
                            emit("    and rax, rbx");
                        else if (bin->op == "|")
                            emit("    or rax, rbx");
                        else if (bin->op == "^")
                            emit("    xor rax, rbx");
                        else if (bin->op == "<<")
                        {
                            emit("    mov rcx, rbx");
                            emit("    shl rax, cl");
                        }
                        else if (bin->op == ">>")
                        {
                            emit("    mov rcx, rbx");
                            emit("    sar rax, cl"); 
                        }
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
                else if (un->op == "~") 
                    emit("    not rax");
                else if (un->op == "++") 
                {
                    if (auto id = std::dynamic_pointer_cast<IdentifierExpr>(un->rhs))
                    {
                        emit("    inc rax");
                        int off = varTable[id->name];
                        emit("    mov " + slot(off) + ", rax");
                    }
                }
                else if (un->op == "--") 
                {
                    if (auto id = std::dynamic_pointer_cast<IdentifierExpr>(un->rhs))
                    {
                        emit("    dec rax");
                        int off = varTable[id->name];
                        emit("    mov " + slot(off) + ", rax");
                    }
                }
            }
            else if (auto ifExpr = std::dynamic_pointer_cast<IfExpr>(expr))
            {
                std::string elseLbl = newLabel("if_expr_else");
                std::string doneLbl = newLabel("if_expr_done");

                std::string thenType = getExprType(ifExpr->thenBranch);
                std::string elseType = getExprType(ifExpr->elseBranch);
                bool resultIsFloat = (thenType == "float" || elseType == "float");

                genExpr(ifExpr->condition);
                emit("    cmp rax, 0");
                emit("    je " + elseLbl);

                genExpr(ifExpr->thenBranch);
                if (resultIsFloat && thenType != "float")
                {

                    emit("    cvtsi2sd xmm0, rax");
                    emit("    movq rax, xmm0");
                }
                emit("    jmp " + doneLbl);

                emit(elseLbl + ":");
                genExpr(ifExpr->elseBranch);
                if (resultIsFloat && elseType != "float")
                {

                    emit("    cvtsi2sd xmm0, rax");
                    emit("    movq rax, xmm0");
                }

                emit(doneLbl + ":");

            }
            else if (auto call = std::dynamic_pointer_cast<CallExpr>(expr))
            {

                for (int i = call->args.size() - 1; i >= 0; i--)
                {
                    genExpr(call->args[i]);
                    emit("    push rax");
                }

                if (auto callee = std::dynamic_pointer_cast<IdentifierExpr>(call->callee))
                {
                    emit("    call " + callee->name);

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

            else if (auto arr = std::dynamic_pointer_cast<ArrayExpr>(expr))
            {
                handleArrayExpr(arr);
            }

            else if (auto idx = std::dynamic_pointer_cast<IndexExpr>(expr))
            {
                genExpr(idx->array); 

                emitNullCheck("rax");

                emit("    push rax");     
                genExpr(idx->index);      
                emit("    mov rbx, rax"); 
                emit("    pop rax");      

                emitArrayBoundsCheck("rax", "rbx");

                emit("    push rax");                 
                emit("    push rbx");                 
                emit("    mov rcx, qword [rax + 8]"); 
                emit("    imul rbx, 8");              
                emit("    add rcx, rbx");
                emit("    mov rdx, qword [rcx]"); 
                emit("    pop rbx");              
                emit("    pop rax");              

                emit("    imul rbx, 8");
                emit("    add rbx, 16"); 
                emit("    add rax, rbx");
                emit("    mov rax, qword [rax]"); 

            }

            else if (auto assign = std::dynamic_pointer_cast<ArrayAssignExpr>(expr))
            {
                genExpr(assign->array); 
                emitNullCheck("rax");

                emit("    push rax");     
                genExpr(assign->index);   
                emit("    mov rbx, rax"); 
                emit("    pop rax");      

                emitArrayBoundsCheck("rax", "rbx");

                emit("    push rax");     
                emit("    push rbx");     
                genExpr(assign->value);   
                emit("    mov rcx, rax"); 
                emit("    pop rbx");      
                emit("    pop rax");      

                emit("    imul rbx, 8");
                emit("    add rbx, 8");
                emit("    add rax, rbx");
                emit("    mov qword [rax], rcx"); 
                emit("    mov rax, rcx");         
            }
        }
    };
}