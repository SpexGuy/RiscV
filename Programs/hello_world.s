.orig 0x0C00         ; processor execution starts at 0x0C00
    LI   x4, 0x10000 ; address of output device
    LA   x2, MESSAGE ; pointer into string
LOOP
    LBU  x3, x2      ; load current char
    BEQZ x3, DONE    ; check if '\0'
    ADDI x2, x2, 1   ; increment string ptr
    SB   x3, x4      ; write char to output
    J LOOP           ; loop
DONE
    EBREAK           ; unhandled trap - will stop simulator
    J DONE           ; loop forever just in case


MESSAGE
    .stringz "Hello, RISC-V!\n"
