#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <assert.h>
#include "types.h"
#include "rv32i.h"
#include "processor.h"

#define EXIT_BAD_USAGE 1
#define EXIT_BAD_MALLOC -1
#define EXIT_BAD_FILE -2

#define DEBUG_TRAPS 0
#define DEBUG_INSTRUCTIONS 0
#define DEBUG_REGISTER_WRITE 0

#define MEM_SIZE 65536
u8 mem[MEM_SIZE];

typedef u32 ureg;
typedef s32 sreg;

struct {
    ureg registers[32];
    ureg pc;

    // CSRs
    u64 instret;
    u64 clocks;
} cpu;

internal
u32 getWord(u32 addr) {
    u32 result = * (u32 *) (mem + addr);
    return result;
}

internal
void setRd(u32 word, ureg val) {
    u32 rd = getRd(word);
    if (DEBUG_REGISTER_WRITE) {
        printf("  x%d = 0x%08X\n", rd, val);
    }
    cpu.registers[rd] = val;
}

internal
ureg readRs1(u32 word) {
    ureg rs1 = getRs1(word);
    ureg val = cpu.registers[rs1];
    sreg mask = -(rs1 != 0);
    return val & mask;
}

internal
ureg readRs2(u32 word) {
    ureg rs2 = getRs2(word);
    ureg val = cpu.registers[rs2];
    sreg mask = -(rs2 != 0);
    return val & mask;
}

internal
ureg loadByteSext(u32 addr, s32 *trap) {
    sreg value = 0;
    if (addr < 0x400 || addr >= MEM_SIZE) {
        *trap = TRAP_UNMAPPED_ADDRESS;
    } else {
        s8 sByte = * (s8 *) (mem + addr);
        value = (sreg) sByte;
    }
    return (ureg) value;
}

internal
ureg loadHalfSext(u32 addr, s32 *trap) {
    sreg value = 0;
    if (addr < 0x400 || addr >= MEM_SIZE - 1) {
        *trap = TRAP_UNMAPPED_ADDRESS;
    } else {
        s16 sPart = * (s16 *) (mem + addr);
        value = (sreg) sPart;
    }
    return (ureg) value;
}

internal
ureg loadWord(u32 addr, s32 *trap) {
    ureg value = 0;
    if (addr < 0x400 || addr >= MEM_SIZE - 3) {
        *trap = TRAP_UNMAPPED_ADDRESS;
    } else {
        value = * (u32 *) (mem + addr);
    }
    return value;
}

internal
ureg loadByte(u32 addr, s32 *trap) {
    ureg value = 0;
    if (addr < 0x400 || addr >= MEM_SIZE) {
        *trap = TRAP_UNMAPPED_ADDRESS;
    } else {
        value = * (u8 *) (mem + addr);
    }
    return value;
}

internal
ureg loadHalf(u32 addr, s32 *trap) {
    ureg value = 0;
    if (addr < 0x400 || addr >= MEM_SIZE) {
        *trap = TRAP_UNMAPPED_ADDRESS;
    } else {
        value = * (u16 *) (mem + addr);
    }
    return value;
}

internal
s32 storeByte(u32 addr, u32 value) {
    s32 trap = -1;
    if (addr == 0x10000) {
        putc(value, stdout);
    } else if (addr < 0x400 || addr >= MEM_SIZE) {
        trap = TRAP_UNMAPPED_ADDRESS;
    } else {
        * (u8 *) (mem + addr) = (u8)value;
    }
    return trap;
}

internal
s32 storeHalf(u32 addr, u32 value) {
    s32 trap = -1;
    if (addr < 0x400 || addr >= MEM_SIZE - 1) {
        trap = TRAP_UNMAPPED_ADDRESS;
    } else {
        * (u16 *) (mem + addr) = (u16)value;
    }
    return trap;
}

internal
s32 storeWord(u32 addr, u32 value) {
    s32 trap = -1;
    if (addr < 0x400 || addr >= MEM_SIZE - 3) {
        trap = TRAP_UNMAPPED_ADDRESS;
    } else {
        * (u32 *) (mem + addr) = (u32)value;
    }
    return trap;
}

#define CSR_OP_WRITE 0
#define CSR_OP_SET   1
#define CSR_OP_CLEAR 2

internal
s32 doCsr(u32 inst, u32 in, u32 op, bool imm) {
    s32 trap = -1;

    bool canRead = false;
    bool canWrite = false;

    u64 value = 0;
    u64 setValue = 0;

    u32 csr = getCSR(inst);
    bool high = (csr & CSR_HIGH) != 0;

    // read the CSR
    switch (csr & ~CSR_HIGH) {

    case CSR_CYCLE: {
        canRead = true;
        value = cpu.clocks;
    } break;

    case CSR_TIME: {
        canRead = true;
        value = time(0);
    } break;

    case CSR_INSTRET: {
        canRead = true;
        value = cpu.instret;
    } break;

    }

    u32 regVal = high ? (u32)(value >> 32) : (u32)(value);
    u32 setRegVal = regVal;

    bool wantsWrite = true;
    bool wantsRead = true;

    // validate ops
    switch (op) {
    case CSR_OP_WRITE:
        wantsWrite = true;
        wantsRead = getRd(inst) != 0;
        setRegVal = in;
        break;
    case CSR_OP_SET:
        wantsRead = true;
        wantsWrite = imm ? (value != 0) : (getRs1(inst) != 0);
        setRegVal |= in;
        break;
    case CSR_OP_CLEAR:
        wantsRead = true;
        wantsWrite = imm ? (value != 0) : (getRs1(inst) != 0);
        setRegVal &= ~in;
        break;
    default:
        assert(false);
        break;
    }

    if (wantsRead && !canRead) {
        trap = TRAP_ILLEGAL_CSR_READ;
    } else if (wantsWrite && !canWrite) {
        trap = TRAP_ILLEGAL_CSR_WRITE;
    } else if (wantsWrite) {
        // write the CSR
        u64 result;
        if (high) {
            result = ((u64)setRegVal << 32) | (value & 0xFFFFFFFFULL);
        } else {
            result = (value & 0xFFFFFFFF00000000ULL) | ((u64)setRegVal);
        }

        switch (csr & ~CSR_HIGH) {
        // TODO: CSR writes go here

        }
    }

    if (wantsRead && trap == -1) {
        setRd(inst, regVal);
    }

    return trap;
}


internal
void doOp(u32 inst, ureg a, ureg b, u32 mask) {
    ureg result = 0;

    switch (getF3(inst)) {
    case F3_SLT:  result = (sreg)a < (sreg)b; break;
    case F3_SLTU: result = (ureg)a < (ureg)b; break;
    case F3_XOR:  result = a ^ b; break;
    case F3_OR:   result = a | b; break;
    case F3_AND:  result = a & b; break;

    case F3_ADD:  {
        if (inst & mask & WORD_REV) result = a - b;
        else                        result = a + b;
    } break;

    case F3_SLL: {
        ureg shift = getRs2(inst);
        result = a << shift;
    } break;
    
    case F3_SRL: {
        ureg shift = getRs2(inst);
        if (inst & WORD_REV) result = (sreg)a >> shift;
        else                 result = (ureg)a >> shift;
    } break;
    }

    setRd(inst, result);
}

void executeInstruction(u32 inst) {
    s32 trap = -1;
    u32 opcode = getOpcode(inst);
    ureg nextPC = cpu.pc + 4;

    switch (opcode) {

    case OP_LUI: {
        setRd(inst, inst & 0xFFFFF000);
    } break;

    case OP_AUIPC: {
        setRd(inst, (inst & 0xFFFFF000) + cpu.pc);
    } break;

    case OP_JAL: {
        setRd(inst, nextPC);
        nextPC = cpu.pc + getImmJAL(inst);
    } break;

    case OP_JALR: {
        setRd(inst, nextPC);
        nextPC = readRs1(inst) + getImm12(inst);
    } break;

    case OP_BRANCH: {
        ureg a = readRs1(inst);
        ureg b = readRs2(inst);
        ureg branchPC = cpu.pc + getImmBranch(inst);
        ureg cond = 0;
        switch (getF3(inst)) {
        case F3_BEQ:  cond = (a == b); break;
        case F3_BNE:  cond = (a != b); break;
        case F3_BLT:  cond = ((sreg)a <  (sreg)b); break;
        case F3_BGE:  cond = ((sreg)a >= (sreg)b); break;
        case F3_BLTU: cond = (a <  b); break;
        case F3_BGEU: cond = (a >= b); break;
        default: trap = TRAP_INVALID_INSTRUCTION;
        }

        sreg mask = -cond;
        nextPC = (mask & branchPC) | (~mask & nextPC);
    } break;

    case OP_L: {
        u32 addr = readRs1(inst) + getImm12(inst);

        ureg val;
        switch (getF3(inst)) {
        case SIZE_B:  val = loadByteSext(addr, &trap); break;
        case SIZE_H:  val = loadHalfSext(addr, &trap); break;
        case SIZE_W:  val = loadWord(addr, &trap); break;
        case SIZE_BU: val = loadByte(addr, &trap); break;
        case SIZE_HU: val = loadHalf(addr, &trap); break;
        default: trap = TRAP_INVALID_INSTRUCTION;
        }

        if (trap == -1) {
            setRd(inst, val);
        }
    } break;

    case OP_S: {
        u32 addr = readRs1(inst) + getImmStore(inst);
        ureg val = readRs2(inst);

        switch (getF3(inst)) {
        case SIZE_B:  trap = storeByte(addr, val); break;
        case SIZE_H:  trap = storeHalf(addr, val); break;
        case SIZE_W:  trap = storeWord(addr, val); break;
        default: trap = TRAP_INVALID_INSTRUCTION;
        }
    } break;

    case OP_IMM: {
        ureg a = readRs1(inst);
        ureg imm = getImm12(inst);
        doOp(inst, a, imm, 0x0);
    } break;

    case OP: {
        ureg a = readRs1(inst);
        ureg b = readRs2(inst);
        doOp(inst, a, b, 0xFFFFFFFF);
    } break;

    case OP_FENCE: {
        printf("  FENCE\n");
    } break;

    case OP_SYSTEM: {
        if (inst == OP_SYSTEM) {
            trap = TRAP_ECALL;
        } else if (inst == (OP_SYSTEM | 0x00100000)) {
            trap = TRAP_EBREAK;
        } else {
            switch (getF3(inst)) {
            case F3_RW: trap = doCsr(inst, readRs1(inst), CSR_OP_WRITE, false); break;
            case F3_RS: trap = doCsr(inst, readRs1(inst), CSR_OP_SET  , false); break;
            case F3_RC: trap = doCsr(inst, readRs1(inst), CSR_OP_CLEAR, false); break;
            case F3_RWI: trap = doCsr(inst, readRs1(inst), CSR_OP_WRITE, true); break;
            case F3_RSI: trap = doCsr(inst, readRs1(inst), CSR_OP_SET  , true); break;
            case F3_RCI: trap = doCsr(inst, readRs1(inst), CSR_OP_CLEAR, true); break;
            default: trap = TRAP_INVALID_INSTRUCTION;
            }
        }
    } break;

    default: {
        trap = TRAP_INVALID_INSTRUCTION;
    } break;

    }

    if (trap == -1) {
        cpu.pc = nextPC;
        cpu.instret++;
    } else {
        // TODO: set trap CSRs
        cpu.pc = getWord(TRAP_TABLE_BASE + (trap << 2));
        if (DEBUG_TRAPS) {
            printf("  TRAP %02X -> %04X\n", trap, cpu.pc);
        }
    }
}

void execute() {
    cpu.pc = STARTUP_PC;
    while (cpu.pc != 0) {
        cpu.clocks++;
        if (cpu.pc < 0xC00 || cpu.pc >= MEM_SIZE) {
            // can't execute here
            // TODO: set trap CSRs
            cpu.pc = getWord(TRAP_TABLE_BASE + TRAP_UNMAPPED_PC);
        } else if (cpu.pc & 0x3) {
            // bad alignment
            // TODO: set trap CSRs
            cpu.pc = getWord(TRAP_TABLE_BASE + TRAP_BAD_PC_ALIGN);
        } else {
            u32 inst = getWord(cpu.pc);
            if (DEBUG_INSTRUCTIONS) {
                printf("%04X: ", cpu.pc);
                writeWord(inst, cpu.pc);
            }
            executeInstruction(inst);
        }
    }
}

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: simulator <programfile>\n");
        exit(EXIT_BAD_USAGE);
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        printf("Couldn't open input file %s\n", argv[1]);
        exit(EXIT_BAD_FILE);
    }

    fseek(f, 0, SEEK_END);
    u64 length = ftell(f);
    fseek (f, 0, SEEK_SET);
    if (length == 0) {
        printf("File %s is empty\n", argv[1]);
        exit(EXIT_BAD_FILE);
    }

    if (length > MEM_SIZE) {
        printf("File %s is too large - maximum size is %d bytes\n", argv[1], MEM_SIZE);
        exit(EXIT_BAD_FILE);
    }

    fread(mem, 1, length, f);
    fclose(f);

    execute();
}
