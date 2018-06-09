#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "types.h"
#include "rv32i.h"

#define EXIT_BAD_USAGE 1
#define EXIT_BAD_MALLOC -1
#define EXIT_BAD_FILE -2

#define MEM_SIZE 65536
u8 mem[MEM_SIZE];

internal
u32 getWord(u32 addr) {
    u32 result = * (u32 *) (mem + addr);
    return result;
}

void writeProgram() {
    bool writing = false;
    for (u32 pos = 0; pos < MEM_SIZE; pos += 4) {
        u32 word = getWord(pos);
        if (word != 0) {
            writing = true;
            printf("%04X: ", pos);
            writeWord(word, pos);
        } else {
            if (writing) printf("\n");
            writing = false;
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

    writeProgram();
}
