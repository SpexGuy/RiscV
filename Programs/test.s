.orig 0x0100

TRAP_TABLE
    .fillw 0x1000
    .fillw 0x3200 0x2400
    .fillh 0x1200
    .fillb 0x32
    .fillb 24
    .fillb -10
    .fillw 0xCCFF0000

    .filla TRAP_TABLE
    .filla LABEL
    .filla LABEL2
    .filla LABEL3
    .filla LABEL4
    .filla STRING

STRING .stringz "foobar_bazquux"

.orig 0x0300
ORIG
    ADD   x1, x2, x3
    SUB   x1, x2, x3
    SLL   x1, x2, x3
    SLT   x1, x2, x3
    SLTU  x1, x2, x3
    XOR   x1, x2, x3
    SRL   x1, x2, x3
    SRA   x1, x2, x3
    OR    x1, x2, x3
    AND   x1, x2, x3
    .fillw 0
    ADDI  x1, x2, -5
    ADDI  x1, x2, 5
    SLTI  x1, x2, 32
    SLTIU x1, x2, -6
    XORI  x1, x3, 15
    ORI   x3, x5, 0xCCC
    ANDI  x6, t0, 0xFFF
    SLLI  x7, x0, 5
    SRLI  x8, x9, 31
    SRAI  x10, x11, 7
    .fillw 0
    LUI   x4, 0xFFFFF000
    AUIPC x5, 0xFFFFF000
    JAL x31, ORIG
    BEQ x5 x6 ORIG
    JALR x1, x2, 32
    BNE x6 x7 ORIG
    BLT x7 x8 ORIG
    BGE x8 x9 ORIG
    BLTU x9 x10 ORIG
    BGEU x10 x11 ORIG
    .fillw 0
    LB x5 x6 32
    LW x6 x7 30
    LH x7 x8 28
    LBU x8 x9 26
    LHU x9 x10 25
    SB x1 x3 24
    SH x2 x4 23
    SW x3 x5 22
    .fillw 0
    FENCE
    FENCE.I
    .fillw 0
    ECALL
    EBREAK
    .fillw 0
    CSRRW x2 cycle x3
    CSRRS x3 time x4
    CSRRC x4 instret x5
    CSRRWI x5 cycleh -6
    CSRRSI x6 timeh 24
    CSRRCI x7 instreth 15
    .fillw 0
    la x7 ORIG
    lb x8 ORIG
    lw x9 ORIG
    lh x10 ORIG
    sb x11 ORIG t0
    sh x12 ORIG t1
    sw x13 ORIG t2
    .fillw 0
    nop
    li x1 0xABCDEF12
    li x2 0x00000F12
    li x3 0xABCDE000
    mv x4 x5
    not x5 x6
    neg x6 x7
    seqz x7 x8
    snez x8 x9
    sltz x9 x10
    sgtz x10 x11
    .fillw 0
    beqz x1 ORIG
    bnez x2 ORIG
    blez x3 ORIG
    bgez x4 ORIG
    bltz x5 ORIG
    bgtz x6 ORIG
    .fillw 0
    bgt x7 x8 ORIG
    ble x8 x9 ORIG
    bgtu x9 x10 ORIG
    bleu x10 x11 ORIG
    j ORIG
    jal ORIG
    jr x6
    jalr x7
    ret
    call ORIG
    tail ORIG
    .fillw 0
    rdinstret x0
    rdcycle x10
    rdtime x20
    rdinstreth x1
    rdcycleh x11
    rdtimeh x21
    csrr x3 instret
    csrw cycle x4
    csrs time x5
    csrc instreth x6
    csrwi cycleh 14
    csrsi timeh 15
    csrci instreth 16
    .fillw 0


.blkb 127
LABEL

LABEL2 addi x2, x0, 43

LABEL3
LABEL4 .orig 0x4000
