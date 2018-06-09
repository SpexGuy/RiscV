// This file contains defines and utilities for working with the rv32i ISA
#ifndef RV32I_H
#define RV32I_H

#include "types.h"

#define F7_NORM 0x0
#define F7_REV 0x20
#define WORD_REV 0x20000000

#define F3_ADD 0x0
#define F3_SLL 0x1
#define F3_SLT 0x2
#define F3_SLTU 0x3
#define F3_XOR 0x4
#define F3_SRL 0x5
#define F3_OR 0x6
#define F3_AND 0x7

#define F3_BEQ 0x0
#define F3_BNE 0x1
#define F3_BLT 0x4
#define F3_BGE 0x5
#define F3_BLTU 0x6
#define F3_BGEU 0x7

#define F3_RW 0x1
#define F3_RS 0x2
#define F3_RC 0x3
#define F3_RWI 0x5
#define F3_RSI 0x6
#define F3_RCI 0x7

#define SIZE_B 0x0
#define SIZE_H 0x1
#define SIZE_W 0x2
#define SIZE_BU 0x4
#define SIZE_HU 0x5

#define OP_IMM 0x13
#define OP_AUIPC 0x17
#define OP 0x33
#define OP_LUI 0x37
#define OP_BRANCH 0x63
#define OP_JALR 0x67
#define OP_JAL 0x6F
#define OP_L 0x03
#define OP_S 0x23
#define OP_FENCE 0x0F
#define OP_SYSTEM 0x73

#define ECALL     0x00000073
#define EBREAK    0x00100073
#define FENCE_I   0x0000100F
#define FENCE_ALL 0x0FF0000F
#define NOP       0x00000013
#define RET       0x00008067  // JALR x0, x1, 0

#define CSR_CYCLE    0xC00
#define CSR_TIME     0xC01
#define CSR_INSTRET  0xC02
#define CSR_CYCLEH   0xC80
#define CSR_TIMEH    0xC81
#define CSR_INSTRETH 0xC82

#define CSR_HIGH     0x080

#define internal static inline

internal
u32 getOpcode(u32 word) {
    return word & 0x7F;
}

internal
u32 getRd(u32 word) {
    return (word >> 7) & 0x1F;
}

internal
u32 getRs1(u32 word) {
    return (word >> 15) & 0x1F;
}

internal
u32 getRs2(u32 word) {
    return (word >> 20) & 0x1F;
}

internal
u32 getF3(u32 word) {
    return (word >> 12) & 0x7;
}

internal
u32 getF7(u32 word) {
    return (word >> 25) & 0x7F;
}

internal
s32 getImm12(u32 word) {
    u32 base = (word >> 20) & 0x00000FFF;
    u32 sext = (word & 0x80000000) ? 0xFFFFF000 : 0;
    return base | sext;
}

internal
s32 getImmJAL(u32 word) {
    // 20|10:1|11|19:12
    u32 base =
        (word & 0x7FE00000) >> 20 |
        (word & 0x00100000) >> 9  |
        (word & 0x000FF000);
    u32 sext = (word & 0x80000000) ? 0xFFF00000 : 0;
    return base | sext;
}

internal
s32 getImmBranch(u32 word) {
    u32 result =
        ((word & 0x80000000) >> 19) |
        ((word & 0x7E000000) >> 20) |
        ((word & 0x00000F00) >> 7 ) |
        ((word & 0x00000080) << 4 );
    u32 sext = (word & 0x80000000) ? 0xFFFFE000 : 0;
    return result | sext;
}

internal
s32 getImmStore(u32 word) {
    u32 result =
        ((word & 0xFE000000) >> 20) |
        ((word & 0x00000F80) >> 7 );
    u32 sext = (word & 0x80000000) ? 0xFFFFF000 : 0;
    return result | sext;
}

internal
u32 getCSR(u32 word) {
    return (word >> 20) & 0x00000FFF;
}

internal
void writeFill(u32 word) {
    printf("  .fill 0x%08X\n", word);
}

internal
void writeWord(u32 word, u32 address) {
    u32 opcode = getOpcode(word);
    switch (opcode) {

    case OP_LUI:
        printf("  LUI   x%d 0x%08X\n", getRd(word), word & 0xFFFFF000);
        break;

    case OP_AUIPC:
        printf("  AUIPC x%d 0x%08X\n", getRd(word), word & 0xFFFFF000);
        break;

    case OP_JAL:
        printf("  JAL   x%d <0x%04X>\n", getRd(word), address + getImmJAL(word));
        break;

    case OP_JALR:
        printf("  JALR  x%d x%d %d\n", getRd(word), getRs1(word), getImm12(word));
        break;

    case OP_BRANCH: {
        u32 rs1 = getRs1(word);
        u32 rs2 = getRs2(word);
        s32 offset = getImmBranch(word);
        const char *inst = 0;
        switch (getF3(word)) {
        case F3_BEQ:  inst = "BEQ "; break;
        case F3_BNE:  inst = "BNE "; break;
        case F3_BLT:  inst = "BLT "; break;
        case F3_BGE:  inst = "BGE "; break;
        case F3_BLTU: inst = "BLTU"; break;
        case F3_BGEU: inst = "BGEU"; break;
        }

        if (inst) {
            u32 target = address + offset;
            printf("  %s  x%d x%d <0x%04X>\n", inst, rs1, rs2, target);
        } else {
            writeFill(word);
        }
    } break;

    case OP_L: {
        u32 rs1 = getRs1(word);
        u32 rd = getRd(word);
        s32 imm12 = getImm12(word);
        const char *inst = 0;
        switch (getF3(word)) {
        case SIZE_B:  inst = "LB "; break;
        case SIZE_H:  inst = "LH "; break;
        case SIZE_W:  inst = "LW "; break;
        case SIZE_BU: inst = "LBU"; break;
        case SIZE_HU: inst = "LHU"; break;
        }

        if (inst) {
            printf("  %s   x%d x%d %d\n", inst, rd, rs1, imm12);
        } else {
            writeFill(word);
        }
    } break;

    case OP_S: {
        u32 rs1 = getRs1(word);
        u32 rs2 = getRs2(word);
        s32 imm12 = getImmStore(word);
        const char *inst = 0;
        switch (getF3(word)) {
        case SIZE_B:  inst = "SB "; break;
        case SIZE_H:  inst = "SH "; break;
        case SIZE_W:  inst = "SW "; break;
        }

        if (inst) {
            printf("  %s   x%d x%d %d\n", inst, rs2, rs1, imm12);
        } else {
            writeFill(word);
        }
    } break;

    case OP_IMM: {
        u32 rs1 = getRs1(word);
        u32 rd = getRd(word);
        s32 imm = getImm12(word);
        u32 f7 = getF7(word);
        bool shift = false;
        const char *inst = 0;
        switch (getF3(word)) {
        case F3_ADD:  inst = "ADDI "; break;
        case F3_SLT:  inst = "SLTI "; break;
        case F3_SLTU: inst = "SLTIU"; break;
        case F3_XOR:  inst = "XORI "; break;
        case F3_OR:   inst = "ORI  "; break;
        case F3_AND:  inst = "ANDI "; break;
        
        case F3_SLL: {
            imm = getRs2(word);
            inst = "SLLI ";
        } break;
        
        case F3_SRL: {
            imm = getRs2(word);
            if (f7 & F7_REV) inst = "SRAI ";
            else inst = "SRLI ";
        } break;
        }

        if (inst) {
            printf("  %s x%d x%d %d\n", inst, rd, rs1, imm);
        } else {
            writeFill(word);
        }
    } break;

    case OP: {
        u32 rs1 = getRs1(word);
        u32 rs2 = getRs2(word);
        u32 rd = getRd(word);
        u32 f3 = getF3(word);
        u32 f7 = getF7(word);
        const char *inst = 0;
        switch (f3) {
        case F3_ADD: {
            if (f7 & F7_REV) inst = "SUB  ";
            else inst = "ADD  ";
        } break;
        case F3_SRL: {
            if (f7 & F7_REV) inst = "SRA  ";
            else inst = "SRL  ";
        } break;
        case F3_SLL:  inst = "SLL  "; break;
        case F3_SLT:  inst = "SLT  "; break;
        case F3_SLTU: inst = "SLTU "; break;
        case F3_XOR:  inst = "XOR  "; break;
        case F3_OR:   inst = "OR   "; break;
        case F3_AND:  inst = "AND  "; break;
        }

        if (inst) {
            printf("  %s x%d x%d x%d\n", inst, rd, rs1, rs2);
        } else {
            writeFill(word);
        }
    } break;

    case OP_FENCE: {
        printf("  FENCE\n");
    } break;

    case OP_SYSTEM: {
        if (word == OP_SYSTEM) {
            printf("  ECALL\n");
        } else if (word == (OP_SYSTEM | 0x00100000)) {
            printf("  EBREAK\n");
        } else {
            bool imm = false;
            const char *inst = 0;
            switch (getF3(word)) {
            case F3_RW: inst = "CSRRW"; break;
            case F3_RS: inst = "CSRRS"; break;
            case F3_RC: inst = "CSRRC"; break;
            case F3_RWI: inst = "CSRRWI"; imm = true; break;
            case F3_RSI: inst = "CSRRSI"; imm = true; break;
            case F3_RCI: inst = "CSRRCI"; imm = true; break;
            }

            if (inst) {
                if (imm) {
                    printf("  %s x%d 0x%X 0x%X\n", inst, getRd(word), getCSR(word), getRs1(word));
                } else {
                    printf("  %s x%d 0x%X x%d\n", inst, getRd(word), getCSR(word), getRs1(word));
                }
            } else {
                writeFill(word);
            }
        }
    } break;

    default: {
        writeFill(word);
    } break;

    }
}

#endif
