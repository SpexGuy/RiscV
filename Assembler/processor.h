// This file contains defines pertaining to this specific processor
#ifndef PROCESSOR_H
#define PROCESSOR_H

#define TRAP_TABLE_BASE 0x400
#define INT_TABLE_BASE  0x800
#define STARTUP_PC      0xC00

#define TRAP_INVALID_INSTRUCTION 0x01 // value of the instruction is invalid
#define TRAP_BAD_PC_ALIGN 0x02 // alignment of the program counter is invalid
#define TRAP_UNMAPPED_PC 0x03 // position of the program counter is invalid

#define TRAP_UNMAPPED_ADDRESS 0x10 // read or write was attempted to an unmapped address

#define TRAP_ILLEGAL_CSR_READ 0x18 // read was attempted on a write-only or nonexistent CSR
#define TRAP_ILLEGAL_CSR_WRITE 0x19 // write was attempted on a read-only or nonexistent CSR

#define TRAP_ECALL 0x20 // called on the ECALL instruction
#define TRAP_EBREAK 0x21 // called on the EBREAK instruction

#endif
