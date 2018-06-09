## Immediate I-types:
    12 bits immediate
    5 bits src1
    3 bits function
    5 bits dst
    7 bits opcode

### Opcode OP-IMM
These instructions have the same opcode, with differing function bits

ADDI rd, rs1, #imm12 ; add immediate
    rd = rs1 + sext(imm12)

SLTI rd, rs1, #imm12 ; set less than immediate signed
    rd = (rs1 < (signed) sext(imm12)) ? 1 : 0

SLTIU rd, rs1, #imm12 ; set less than immediate unsigned
    rd = (rs1 < (unsigned) sext(imm12)) ? 1 : 0
    Note that the immediate is still sign-extended even though
    the comparison is unsigned.

XORI rd, rs1, #imm12 ; xor immediate
    rd = rs1 ^ sext(imm12)

ORI rd, rs1, #imm12 ; or immediate
    rd = rs1 | sext(imm12)

ANDI rd, rs1, #imm12 ; and immediate
    rd = rs1 & sext(imm12)

SLLI rd, rs1, #imm5 ; shift left logical immediate
    rd = rs1 << zext(imm5)

SRLI rd, rs1, #imm5 ; shift right logical immediate
    rd = rs1 >>> zext(imm5)

SRAI rd, rs1, #imm5 ; shift right arithmetic immediate
    rd = rs1 >> zext(imm5)
    A bit in the unused higher immediate section distinguishes this
    operation from SRLI; they have the same opcode and function bits

### Special I-Type opcodes

JALR rd, rs1, #imm12 ; jump and link register
    rd = pc + 4; pc = (rs1 + sext(imm12)) & ~1
    rd can be x0 if the result is not needed.
    Generates misaligned instruction fetch exception if the target is not 4-byte aligned (except when using compressed instructions)

LW rd, rs1, #imm12, #function ; load word
    rd = mem[rs1 + imm12] @ 4 bytes;

LH rd, rs1, #imm12, #function ; load half
    rd = sext(mem[rs1 + imm12] @ 2 bytes);

LHU rd, rs1, #imm12, #function ; load half unsigned
    rd = zext(mem[rs1 + imm12] @ 2 bytes);

LB rd, rs1, #imm12, #function ; load byte
    rd = sext(mem[rs1 + imm12] @ 1 byte);

LBU rd, rs1, #imm12, #function ; load byte unsigned
    rd = zext(mem[rs1 + imm12] @ 1 byte);


### System I-Types
For all of these types, the opcode is SYSTEM
For CSR-modifying types, the imm12 bits identify a CSR

CSRRW rd, csr, rs1 ; read/write CSR atomic
    atomic {
        // tmp is necessary in case rd and rs1 are the same
        tmp = rs1;
        if (rd != x0) rd = zext(csr);
        csr = tmp;
    }
    Side-effects of CSR read do not happen if rd is x0

CSRRS rd, csr, rs1 ; read/set bits in CSR atomic
    atomic {
        tmp = rs1;
        rd = zext(csr);
        if (rs1 != x0) csr |= tmp;
    }
    Side-effects of CSR write do not happen if rs1 is x0

CSRRC rd, csr, rs1 ; read/clear bits in CSR atomic
    atomic {
        tmp = rs1;
        rd = zext(csr);
        if (rs1 != r0) csr &= ~tmp;
    }
    Side-effects of CSR write do not happen if rs1 is x0

CSRRWI/CSRRSI/CSRRCI
    like base version but zext the bits of rs1 as immediate
    if the bits are 0 side effects of CSRRC and CSRRS don't happen

ECALL ; trap into an OS call

EBREAK ; transfer control to a debugger



## Upper Immediate U-Types
    20 bits immediate
    5 bits dst
    7 bits opcode

LUI rd, #imm20 ; load upper immediate
    rd = imm20 << 12

AUIPC rd, #imm20 ; add upper 
    rd = pc + imm20 << 12
    All instructions you might use this number with have a 12-bit
    immediate that allows you to specify the remaining 12 bits.


## Register R-Types
    7 bits function2
    5 bits rs2
    5 bits rs1
    3 bits function1
    5 bits rd
    7 bits opcode (always $OP)

ADD rd, rs1, rs2 ; add
    rd = rs1 + rs2

SUB rd, rs1, rs2 ; subtract
    rd = rs? - rs? // TODO: WHICH ORDER
    This is the same function1 as ADD, with bit 30 set in function2

SLT rd, rs1, rs2 ; set less than
    rd = (rs1 < (signed) rs2) ? 1 : 0

SLTU rd, rs1, rs2 ; set less than unsigned
    rd = (rs1 < (unsigned) rs2) ? 1 : 0

AND rd, rs1, rs2 ; and
    rd = rs1 & rs2

OR rd, rs1, rs2 ; or
    rd = rs1 | rs2

XOR rd, rs1, rs2 ; xor
    rd = rs1 ^ rs2

SLL rd, rs1, rs2 ; shift left logical
    rd = rs1 << (rs2 & 0x1F)

SRL rd, rs1, rs2 ; shift right logical
    rd = rs1 >>> (rs2 & 0x1F)

SRA rd, rs1, rs2 ; shift right arithmetic
    rd = rs1 >> (rs2 & 0x1F)
    This is the same function1 as SRL, with bit 30 set in function2

## Branching J-Types
    1 bit immA
    10 bits immB
    1 bit immC
    8 bits immD
    5 bits rd
    7 bits opcode

    imm21 = {immA, immD, immC, immB, 0}

JAL rd, #imm21
    rd = pc + 4; pc = pc + sext(imm21)
    standard calling convention:
        x1 is return address,
        x5 is alternate link register
    Generates misaligned instruction fetch exception if the target is not 4-byte aligned (except when using compressed instructions)


## Branching B-Types
    1 bit immA
    6 bits immB
    5 bits rs2
    5 bits rs1
    3 bits function
    4 bits immC
    1 bit immD
    7 bits opcode (always BRANCH)

    imm13 = {immA, immD, immB, immC, 0}

BEQ rs1, rs2, #imm13 ; branch if equal
    pc = (rs1 == rs2) ? pc + imm13 : pc

BNE rs1, rs2, #imm13 ; branch if not equal
    pc = (rs1 != rs2) ? pc + imm13 : pc

BLT rs1, rs2, #imm13 ; branch if less than
    pc = (rs1 < (signed) rs2) ? pc + imm13 : pc

BLTU rs1, rs2, #imm13 ; branch if less than unsigned
    pc = (rs1 < (unsigned) rs2) ? pc + imm13 : pc

BGE rs1, rs2, #imm13 ; branch if greater than or equal to
    pc = (rs1 >= (signed) rs2) ? pc + imm13 : pc

BGEU rs1, rs2, #imm13 ; branch if greater than or equal to unsigned
    pc = (rs1 >= (unsigned) rs2) ? pc + imm13 : pc


## Storage S-Types
    7 bits immA
    5 bits rs2
    5 bits rs1
    3 bits function (storage width)
    5 bits immB
    7 bits opcode (always STORE)

Aligned loads/stores are atomic.  Unaligned are not necessarily

SW rs2, rs1, #imm12 ; store word
    mem[rs1 + imm12] @ 4 bytes = rs2

SH rs2, rs1, #imm12 ; store half
    mem[rs1 + imm12] @ 2 bytes = rs2 & 0xFFFF

SB rs2, rs1, #imm12 ; store byte
    mem[rs1 + imm12] @ 1 byte = rs2 & 0xFF


## Fence
    4 bits reserved (always zero)
    1 bit PI
    1 bit PO
    1 bit PR
    1 bit PW
    1 bit SI
    1 bit SO
    1 bit SR
    1 bit SW
    5 bits rs1 (always zero)
    3 bits function
    5 bits rd (always zero)
    7 bits opcode (always MISC-MEM)

FENCE <flags>
    Other processors may not observed flagged operations executed
    before this fence before flagged operations executed after
    this fence.

FENCE.I
    Synchronize the instruction and data streams.  Use this before
    executing JITed or otherwise generated code.


## Pseudo-ops:

SEQZ rd, rs ; set equal to zero
    = SLTIU rd, rs, #1
    Sets rd to 1 if rs == 0, or 0 otherwise.

NOT rd, rs ; arithmetic not
    = XORI rd, rs, #-1
    Sets rd to ~rs

SNEZ rd, rs ; set not equal to zero
    = SLTU rd, x0, rs
    Sets rd to 1 if rs != 0, and 0 otherwise

NOP ; no-op
    = ADDI x0, x0, 0
    Has no effect; the processor will move to the next instruction as normal.

J #imm21 ; unconditional jump
    = JAL x0 imm21

BGT r1, r2, #imm13 ; branch if greater than
    = BLT r2, r1, #imm13

BGTU r1, r2, #imm13 ; branch if greater than unsigned
    = BLTU r2, r1, #imm13

BLE r1, r2, #imm13 ; branch if less than or equal to
    = BGE r2, r1, #imm13

BLEU r1, r2, #imm13 ; branch if less than or equal to unsigned
    = BGTU r2, r1, #imm13

CSRR rd, csr ; read a hardware counter
    = CSRRS rd, csr, x0

CSRW csr, rs1 ; write a hardware counter
    = CSRRW x0, csr, rs1

CSRWI csr, #imm5 ; write a hardware counter immediate
    = CSRRWI x0, csr, #imm5

CSRS csr, rs1 ; set bits in a hardware counter
    = CSRRS x0, csr, rs1

CSRSI csr, #imm5 ; set bits in a hardware counter immediate
    = CSRRSI x0, csr, #imm5

CSRC csr, rs1 ; unset bits in a hardware counter
    = CSRRC x0, csr, rs1

CSRCI csr, #imm5 ; unset bits in a hardware counter immediate
    = CSRRCI x0, csr, #imm5

RDCYCLE[H] rd ; read cycle counter, H for high 32 bits
    = CSRR rd, cycle

RDTIME[H] rd ; read wall-clock time counter, H for high 32 bits
    = CSRR rd, time

RDINSTRET[H] rd ; read instructions retired counter, H for high 32 bits
    = CSRR rd, instret


