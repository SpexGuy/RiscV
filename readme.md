# RISC-V Stuff
This project consists of a simple assembler and emulator for the Risc-V architecture, as well as a verilog implementation of a simple 5-stage pipelined Risc-V processor.

This is a work in progress.  The current status of the project can be found at the bottom of this document.

## Building the toolchain
The assembler, disassembler, and emulator can be built using the commands
```
cd Assembler
./build
```
They can then be run using
```
./assemble path/to/program.s
./disassemble path/to/program.o
./simulate path/to/program.o
```
Programs are limited to 64 kiB.

On boot, the processor starts executing at address 0xC00

Memory before address 0x0400 is undefined, and any attempted access will result in an UNMAPPED_ADDRESS trap.

The trap table begins at 0x0400.  Each entry is a 4-byte address for the trap handler.  Trap ids can be found in `Assembler/processor.h`.  The address used for handling a trap is `0x0400 + trap_id * 4`.

## Building the verilog
The verilog parts of this project are built to compile and simulate with icarus verilog and vvp. On mac, you can install these tools via homebrew, using the command
```
brew install icarus-verilog
```
Once icarus is installed, you can build and test the processor from the Verilog directory
```
cd Verilog
./build
./run
```

## Instantiating on an FPGA
Coming soon, hopefully!

## Diagrams:
Single-cycle design:
https://docs.google.com/drawings/d/1b8XIaz4Rt0YtwfL9ZVi_imPRlvix7FpQp_StdsAsH9s/edit?usp=sharing

## Current Status
- [x] assembler/linker
- [x] disassembler
- [x] software emulator, user-mode
- [ ] software emulator, supervisor instructions
- [x] verilog project setup
- [ ] ALU
- [ ] register file
- [ ] ??? other hardware parts ???
- [ ] rv32i base user-mode instructions
- [ ] traps
- [ ] supervisor instructions
- [ ] FPGA instantiation
- [ ] branch prediction, upwards always taken
- [ ] "M" extension for integer multiplication/division
- [ ] ??? a better model of memory where reads can take many clocks ???
- [ ] ??? "F" extension for floating-point ???
- [ ] ??? "N" extension for user-mode interrupts ???
- [ ] ??? "C" extension for compressed instructions ???
- [ ] ??? try to emulate a risc-v linux distro ???
- [ ] ??? upgrade to rv64i ???
- [ ] ??? out-of-order execution ???
