// zinc/src/ir/ir.cpp
#include "ir.h"
#include <iostream>

using namespace ir;

static std::string indent = "  ";

static std::string valToStr(const Value* v) {
    if (!v) return "null";
    if (v->isConst) return v->constVal;
    return v->name;
}

std::string Module::dump() const {
    std::ostringstream out;
    for (auto &fptr : functions) {
        auto &f = *fptr;
        out << "define " << f.retType.str() << " @" << f.name << "() {\n";
        for (auto &bbptr : f.blocks) {
            out << bbptr->name << ":\n";
            for (auto &instPtr : bbptr->instrs) {
                auto &inst = *instPtr;
                out << indent;
                if (!inst.name.empty()) out << inst.name << " = ";
                // op printing
                switch(inst.op) {
                    case Instruction::Op::Const:
                        out << "const " << valToStr(&inst) << "\n";
                        break;
                    case Instruction::Op::Alloca:
                        out << "alloca " << inst.type.str() << "\n";
                        break;
                    case Instruction::Op::Load:
                        out << "load " << valToStr(inst.operands[0]) << "\n";
                        break;
                    case Instruction::Op::Store:
                        out << "store " << valToStr(inst.operands[0]) << ", " << valToStr(inst.operands[1]) << "\n";
                        break;
                    case Instruction::Op::Add:
                    case Instruction::Op::Sub:
                    case Instruction::Op::Mul:
                    case Instruction::Op::Div:
                        {
                            std::string op;
                            if (inst.op==Instruction::Op::Add) op = "add";
                            if (inst.op==Instruction::Op::Sub) op = "sub";
                            if (inst.op==Instruction::Op::Mul) op = "mul";
                            if (inst.op==Instruction::Op::Div) op = "div";
                            out << op << " " << valToStr(inst.operands[0]) << ", " << valToStr(inst.operands[1]) << "\n";
                        }
                        break;
                    case Instruction::Op::Ret:
                        if (!inst.operands.empty()) out << "ret " << valToStr(inst.operands[0]) << "\n";
                        else out << "ret\n";
                        break;
                    case Instruction::Op::Br:
                        out << "br " << inst.target->name << "\n";
                        break;
                    case Instruction::Op::CondBr:
                        out << "brcond " << valToStr(inst.operands[0]) << ", " << inst.target->name << ", " << inst.target2->name << "\n";
                        break;
                    case Instruction::Op::Phi:
                        out << "phi ";
                        for (size_t i=0;i<inst.phiIncoming.size();++i){
                            if (i) out << ", ";
                            out << "[" << valToStr(inst.phiIncoming[i].first) << ", " << inst.phiIncoming[i].second->name << "]";
                        }
                        out << "\n";
                        break;
                    case Instruction::Op::Call:
                        out << "call " << inst.operands.size() << " args\n";
                        break;
                    default:
                        out << "nop\n"; break;
                }
            }
        }
        out << "}\n\n";
    }
    return out.str();
}
