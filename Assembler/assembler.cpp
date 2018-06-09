#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "types.h"
#include "rv32i.h"

#define EXIT_ERRORS 2
#define EXIT_BAD_USAGE 1
#define EXIT_BAD_MALLOC -1
#define EXIT_BAD_FILE -2
#define EXIT_BAD_ADDR -3

enum immediate_type {
    RAW_WORD,
    BRANCH,
    IMM_JAL,
    IMM_CALL,
    IMM_LA,
    FULL_LOAD,
    FULL_STORE
};

#define BAD_CSR 0xF0000000

struct input {
    u32 line;
    u8 *base;
    u8 *position;
    u8 *end;
};

struct token {
    u8 *base;
    u16 len;
};

struct link {
    token tok;
    u32 pos;
    u32 line;
    immediate_type type;
};

struct label_node {
    label_node *next;
    token tok;
    u32 hash;
    u32 pos;
};

#define OUT_SIZE 0x40000
u8 output[OUT_SIZE];
u32 outpos = 0;

#define MAX_LINKS 0x2000
link links[MAX_LINKS];
u32 linkCount = 0;

#define MAX_LABELS 0x800
label_node labels[MAX_LABELS];
u32 labelCount = 0;
u32 processedLabels = 0;

#define LABEL_PRIME 2087
label_node *label_table[LABEL_PRIME];


// ------------------------------- Tokens ------------------------------------

void printToken(token t) {
    for (u32 c = 0; c < t.len; c++) {
        putc(t.base[c], stdout);
    }
}

char lower(char val) {
    if (val >= 'A' && val <= 'Z') {
        val = (val - 'A') + 'a';
    }
    return val;
}

bool is(token tok, const char *val) {
    int n = tok.len;
    int c = 0;
    for (; c < n; c++) {
        if (lower(tok.base[c]) != val[c] || !val[c])
            return false;
    }
    return !val[c];
}

bool is(token a, token b) {
    int n = a.len;
    if (b.len != n) return false;

    for (int c = 0; c < n; c++) {
        if (lower(a.base[c]) != lower(b.base[c]))
            return false;
    }

    return true;
}


// ------------------------------- Errors ------------------------------------

u32 errorCount = 0;

void addError(const char *err, input &in, token tok) {
    printf("ERROR: %s '", err);
    printToken(tok);
    printf("' on line %d\n", in.line);
    errorCount++;
}

void addError(const char *err, input &in) {
    printf("ERROR: %s on line %d\n", err, in.line);
    errorCount++;
}

void addLinkError(const char *err, link &ln) {
    printf("ERROR: %s '", err);
    printToken(ln.tok);
    printf("' on line %d\n", ln.line);
    errorCount++;
}


// ------------------------------- Label Hash Table ------------------------------------

u32 hashLabel(token tok) {
    // use djb2 on string
    u32 hash = 5381;
    for (u32 c = 0; c < tok.len; c++) {
        char val = tok.base[c];
        hash = ((hash << 5) + hash) + val;
    }
    return hash;
}

label_node *getExistingLabel(token tok, u32 hash) {
    u32 index = hash % LABEL_PRIME;
    label_node *node = label_table[index];
    while (node) {
        if (node->hash == hash && is(tok, node->tok)) {
            break;
        }
        node = node->next;
    }
    return node;
}

void addLabel(input &in, token t) {
    assert(labelCount < MAX_LABELS);
    u32 hash = hashLabel(t);
    label_node *existing = getExistingLabel(t, hash);
    if (existing) {
        addError("Duplicate Label", in, t);
    } else {
        label_node *l = &labels[labelCount++];
        l->hash = hash;
        l->tok = t;
    }
}

void processLabels(u32 pos) {
    while (processedLabels < labelCount) {
        label_node *l = &labels[processedLabels++];
        l->pos = pos;
        u32 index = l->hash % LABEL_PRIME;
        l->next = label_table[index];
        label_table[index] = l;
    }
}


// ------------------------------- Output ------------------------------------

void alignTo(u32 align) {
    assert((align & (align-1)) == 0);

    u32 mask = align-1;
    u32 rem = outpos & mask;
    if (rem != 0) {
        outpos += align - rem;
    }
}

void advanceBy(u32 bytes) {
    outpos += bytes;
    assert(outpos < OUT_SIZE);
}

void moveTo(u32 position) {
    assert(position < OUT_SIZE);
    outpos = position;
}

void putWord(u32 pos, u32 word) {
    output[pos+0] = u8(word);
    output[pos+1] = u8(word>>8);
    output[pos+2] = u8(word>>16);
    output[pos+3] = u8(word>>24);
}

u32 getWord(u32 pos) {
    u32 val =
        output[pos+0] +
        (output[pos+1] << 8) +
        (output[pos+2] << 16) +
        (output[pos+3] << 24);
    return val;
}

u32 addWord(u32 word) {
    u32 pos = outpos;
    advanceBy(4);
    putWord(pos, word);
    return pos;
}

u32 addAlignedWord(u32 word) {
    alignTo(4);
    return addWord(word);
}

u32 addHalf(u32 half) {
    u32 pos = outpos;
    advanceBy(2);
    output[pos+0] = u8(half);
    output[pos+1] = u8(half>>8);
    return pos;
}

u32 addByte(u32 byte) {
    u32 pos = outpos;
    advanceBy(1);
    output[pos] = u8(byte);
    return pos;
}

bool checkSext(u32 num, u32 topBit, u32 bottomBit) {
    if (bottomBit != 0) {
        u32 bottomMask = (1 << bottomBit) - 1;
        if (num & bottomMask) return false;
    }

    if (topBit < 32) {
        u32 mask = (1 << (topBit-1)) - 1;
        u32 extBits = ~mask;
        u32 actualExtBits = num & extBits;
        if ((actualExtBits != 0) && (actualExtBits != extBits)) {
            return false;
        }
    }

    return true;
}




// ------------------------------- Links ------------------------------------

void addLink(input &in, token tok, u32 pos, immediate_type type) {
    assert(linkCount < MAX_LINKS);
    link &l = links[linkCount++];
    l.tok = tok;
    l.pos = pos;
    l.type = type;
    l.line = in.line;
}

u32 packImmBranch(u32 offset) {
    u32 result = 
        ((offset & 0x1000) << 19) |
        ((offset & 0x07E0) << 20) |
        ((offset & 0x001E) << 7 ) |
        ((offset & 0x0800) >> 4 );
    return result;
}

u32 packImmJAL(u32 offset) {
    u32 result =
        ((offset & 0x100000) << 11) |
        ((offset & 0x0007FE) << 20) |
        ((offset & 0x000800) << 9 ) |
        ((offset & 0x0FF000));
    return result;
}

u32 packImmStore(u32 offset) {
    u32 result =
        ((offset & 0xFE0) << 20) |
        ((offset & 0x01F) << 7);
    return result;
}

void fillInLink(immediate_type type, u32 codePos, u32 targetPos, link &ln) {
    switch (type) {
    
    case RAW_WORD: {
        putWord(codePos, targetPos);
    } break;
    
    case BRANCH: {
        u32 offset = targetPos - codePos;
        if (!checkSext(offset, 13, 1)) {
            addLinkError("Branch jumps too far away", ln);
        } else {
            u32 mask = packImmBranch(offset);
            u32 instr = getWord(codePos);
            instr |= mask;
            putWord(codePos, instr);
        }
    } break;

    case IMM_JAL: {
        u32 offset = targetPos - codePos;
        if (!checkSext(offset, 21, 1)) {
            addLinkError("JAL jumps too far away", ln);
        } else {
            u32 mask = packImmJAL(offset);
            u32 instr = getWord(codePos);
            instr |= mask;
            putWord(codePos, instr);
        }
    } break;

    // auipc rx upper
    // imm12 rl rx lower
    case FULL_LOAD:
    case IMM_LA:
    case IMM_CALL: {
        u32 offset = targetPos - codePos;

        u32 auipc = getWord(codePos);
        auipc |= (offset + 0x00000800) & 0xFFFFF000;
        putWord(codePos, auipc);

        u32 jalr = getWord(codePos + 4);
        jalr |= (offset << 20);
        putWord(codePos + 4, jalr);
    } break;

    case FULL_STORE: {
        u32 offset = targetPos - codePos;

        u32 auipc = getWord(codePos);
        auipc |= (offset + 0x00000800) & 0xFFFFF000;
        putWord(codePos, auipc);

        u32 st = getWord(codePos + 4);
        st |= packImmStore(offset & 0x00000FFF);
        putWord(codePos + 4, st);
    } break;

    }
}

void resolveLinks() {
    for (u32 c = 0; c < linkCount; c++) {
        link &l = links[c];
        u32 hash = hashLabel(l.tok);
        label_node *node = getExistingLabel(l.tok, hash);
        if (!node) {
            addLinkError("Reference to missing label", l);
            continue;
        }

        fillInLink(l.type, l.pos, node->pos, l);
    }
}

// ------------------------------- Parsing ------------------------------------

bool skipWhitespace(input &in) {
    bool moreInLine = false;
    u8 *pos = in.position;
    u8 *end = in.end;

    // scan forward through whitespace
    while (pos < end) {
        char val = *pos;

        if (val == '\n') {
            break;
        }

        if (val == ';') {
            // skip this comment until a newline
            while (pos < end && *pos != '\n') pos++;
            break;
        }

        if (val != ' ' &&
            val != '\r' &&
            val != '\t' &&
            val != ',') {
            moreInLine = true;
            break;
        }

        pos++;
    }

    in.position = pos;

    return moreInLine;
}

token getToken(input &in) {
    token result = {};

    bool moreInLine = skipWhitespace(in);
    if (moreInLine) {
        u8 *pos = result.base = in.position;
        u8 *end = in.end;

        while (pos < end) {
            char val = *pos;
            if (val == ' ' ||
                val == '\n' ||
                val == '\r' ||
                val == '\t' ||
                val == ',' ||
                val == ';') {
                break;
            }
            pos++;
        }

        in.position = pos;

        assert(pos > result.base);
        result.len = u32(pos - result.base);
    }

    return result;
}

u32 parseNumber(token tok, u32 mask, int &error) {
    u32 num = 0;
    if (tok.len >= 2 && tok.base[0] == '0' && tok.base[1] == 'x') {
        for (u32 c = 2; c < tok.len; c++) {
            num = num << 4;
            char val = tok.base[c];
            if (val >= '0' && val <= '9') {
                num |= (val - '0');
            } else if (val >= 'A' && val <= 'F') {
                num |= 10 + (val - 'A');
            } else if (val >= 'a' && val <= 'f') {
                num |= 10 + (val - 'a');
            } else {
                error = 1;
                return 0;
            }
        }
    } else if (tok.len >= 1) {
        bool negative = tok.base[0] == '-';
        for (u32 c = negative; c < tok.len; c++) {
            num *= 10;
            char val = tok.base[c];
            if (val < '0' || val > '9') {
                error = 1;
                return 0;
            }
            num += val - '0';
        }
        if (negative) num = -num;
    }

    if ((num & ~mask) != 0 &&
        (num | mask) != 0xFFFFFFFF) {
        error = 2;
        return 0;
    }

    error = 0;
    return num;
}

u32 readNumber(input &in, token tok, u32 mask) {
    int error;
    u32 num = parseNumber(tok, mask, error);

    if (error == 1) {
        addError("Invalid numeric literal", in, tok);
        return 0;
    } else if (error == 2) {
        char tmp[1024];
        snprintf(tmp, 1024, "Constant is invalid for mask %08X", mask);
        addError(tmp, in, tok);
        return 0;
    }

    return num;
}

bool isValidFirstLabelChar(char c) {
    return (c >= 'A' && c <= 'Z') ||
            (c >= 'a' && c <= 'z') ||
            (c == '_');
}

bool isValidLabelChar(char c) {
    return isValidFirstLabelChar(c) ||
        (c >= '0' && c <= '9');
}

bool validateLabel(input &in, token tok) {
    if (tok.len == 0) return false;
    if (!isValidFirstLabelChar(tok.base[0])) {
        addError("Invalid label name", in, tok);
        return false;
    }
    for (int c = 1; c < tok.len; c++) {
        if (!isValidLabelChar(tok.base[c])) {
            addError("Invalid label name", in, tok);
            return false;
        }
    }
    return true;
}

void finishEmptyLine(input &in) {
    while (true) {
        token tok = getToken(in);
        if (!tok.base) return;
        addError("Ignoring extra token", in, tok);
    }
}

void addStringLit(input &in) {
    u8 *pos = in.position;
    u8 *end = in.end;
    u32 restore = outpos;

    if (*pos != '"') {
        addError("String literal must start with '\"'", in);
        return;
    }

    pos++;

    bool valid = false;
    bool escaped = false;
    while (pos < end) {
        char val = *pos;
        if (escaped) {
            escaped = false;
            switch (val) {
            case '\\':
                addByte('\\');
                break;
            case 't':
                addByte('\t');
                break;
            case 'n':
                addByte('\n');
                break;
            case '\n':
                in.line++;
                break;
            case '"':
                addByte('"');
                break;
            case '\'':
                addByte('\'');
                break;
            default: {
                char tmp[256];
                snprintf(tmp, 256, "Invalid escape char: \\%c", val);
                addError(tmp, in);
                break;
            }
            }
        } else {
            switch (val) {
            case '\\':
                escaped = true;
                break;
            case '\n':
                addError("String literal missing ending \"", in);
                goto litdone; // don't consume the EOL
            case '"':
                pos++;
                valid = true;
                goto litdone;
            default:
                addByte(val);
                break;
            }
        }
        pos++;
    }
litdone:
    in.position = pos;
    if (!valid) {
        addError("Unexpected EOF in string literal", in);
    } else {
        addByte('\0');
    }
}

const char *abinames[] = {
    "zero", "ra", "sp", "gp",
    "tp", "t0", "t1", "t2",
    "s0", "s1", "a0", "a1",
    "a2", "a3", "a4", "a5",
    "a6", "a7", "s2", "s3",
    "s4", "s5", "s6", "s7",
    "s8", "s9", "s10", "s11",
    "t3", "t4", "t5", "t6"
};

const char *litnames[] = {
    "x0", "x1", "x2", "x3",
    "x4", "x5", "x6", "x7",
    "x8", "x9", "x10", "x11",
    "x12", "x13", "x14", "x15",
    "x16", "x17", "x18", "x19",
    "x20", "x21", "x22", "x23",
    "x24", "x25", "x26", "x27",
    "x28", "x29", "x30", "x31"
};

struct csr_binding {
    const char *name;
    u32 value;
};

csr_binding namedCSRs[] = {
    { "cycle"   , CSR_CYCLE    },
    { "time"    , CSR_TIME     },
    { "instret" , CSR_INSTRET  },
    { "cycleh"  , CSR_CYCLEH   },
    { "timeh"   , CSR_TIMEH    },
    { "instreth", CSR_INSTRETH }
};

#define NELEM(x) (sizeof(x) / sizeof(x[0]))

s32 parseRegister(token tok) {
    if (tok.base[0] == 'x') {
        for (int c = 0; c < NELEM(litnames); c++) {
            if (is(tok, litnames[c])) return c;
        }
    } else {
        for (int c = 0; c < NELEM(abinames); c++) {
            if (is(tok, abinames[c])) return c;
        }
        if (is(tok, "fp")) return 8;
    }

    return -1;
}

u32 parseCSR(token tok) {
    for (int c = 0; c < NELEM(namedCSRs); c++) {
        if (is(tok, namedCSRs[c].name)) {
            return namedCSRs[c].value;
        }
    }

    int error = 0;
    u32 lit = parseNumber(tok, 0x00000FFF, error);
    if (!error) {
        return lit;
    }

    return BAD_CSR;
}

s32 getRegister(input &in) {
    token tok = getToken(in);
    if (!tok.base) {
        addError("Missing register", in);
        return -1;
    }

    s32 reg = parseRegister(tok);
    if (reg == -1) {
        addError("Invalid register name", in, tok);
    }
    return reg;
}

s32 getCSR(input &in) {
    token tok = getToken(in);
    if (!tok.base) {
        addError("Missing csr", in);
        return BAD_CSR;
    }

    u32 csr = parseCSR(tok);
    if (csr == BAD_CSR) {
        addError("Invalid CSR value", in, tok);
    }

    return csr;
}


// ------------------------------- Instruction Formatters ------------------------------------

u32 addRType(u32 op, u32 func3, u32 func7, u32 rd, u32 rs1, u32 rs2) {
    u32 opcode = 
        func7 << 25 |
        rs2   << 20 |
        rs1   << 15 |
        func3 << 12 |
        rd    <<  7 |
        op    <<  0;
    alignTo(4);
    u32 pos = outpos;
    addWord(opcode);
    return pos;
}

u32 addIType(u32 op, u32 func3, u32 rd, u32 rs1, u32 imm12) {
    u32 opcode =
        imm12 << 20 |
        rs1   << 15 |
        func3 << 12 |
        rd    << 7  |
        op    << 0;
    alignTo(4);
    u32 pos = outpos;
    addWord(opcode);
    return pos;
}

u32 addBType(u32 op, u32 func3, u32 rs1, u32 rs2) {
    u32 opcode =
        rs2   << 20 |
        rs1   << 15 |
        func3 << 12 |
        op    << 0;
    alignTo(4);
    u32 pos = outpos;
    addWord(opcode);
    return pos;
}

u32 addUType(u32 op, u32 rd, u32 imm20) {
    u32 opcode =
        (imm20 & 0xFFFFF000) |
        rd << 7 |
        op << 0;
    alignTo(4);
    u32 pos = outpos;
    addWord(opcode);
    return pos;
}

u32 addSimpleRType(input &in, u32 op, u32 func3, u32 func7, u32 dr, u32 sr1, u32 sr2) {
    alignTo(4);
    u32 pos = outpos;
    if (dr != -1 && sr1 != -1 && sr2 != -1) {
        pos = addRType(op, func3, func7, dr, sr1, sr2);
    }
    return pos;
}

u32 addSimpleRType(input &in, u32 op, u32 func3, u32 func7) {
    s32 dr  = getRegister(in);
    s32 sr1 = getRegister(in);
    s32 sr2 = getRegister(in);
    return addSimpleRType(in, op, func3, func7, dr, sr1, sr2);
}

u32 addSimpleIType(input &in, u32 op, u32 func3, u32 rd, u32 rs1, u32 imm12) {
    alignTo(4);
    u32 pos = outpos;
    if (rd != -1 && rs1 != -1) {
        pos = addIType(op, func3, rd, rs1, imm12);
    }
    return pos;
}

u32 addSimpleIType(input &in, u32 op, u32 func3) {
    s32 dr = getRegister(in);
    s32 sr1 = getRegister(in);
    token tok = getToken(in);
    if (!tok.base) {
        addError("No immediate specified", in);
        return outpos;
    }
    u32 imm12 = readNumber(in, tok, 0xFFF);
    return addSimpleIType(in, op, func3, dr, sr1, imm12);
}

u32 addShiftIType(input &in, u32 op, u32 func3, u32 func7) {
    u32 pos = outpos;
    s32 dr = getRegister(in);
    s32 sr1 = getRegister(in);
    token tok = getToken(in);
    if (!tok.base) {
        addError("No immediate specified", in);
        return pos;
    }
    u32 shift = readNumber(in, tok, 0x1F);
    if ((shift & 0x1F) != shift) {
        addError("Invalid shift value", in, tok);
        return pos;
    }
    if (dr != -1 && sr1 != -1) {
        pos = addRType(op, func3, func7, dr, sr1, shift);
    }
    return pos;
}

u32 addSimpleBType(input &in, u32 op, u32 func3, u32 sr1, u32 sr2) {
    alignTo(4);
    u32 pos = outpos;
    token tok = getToken(in);
    if (validateLabel(in, tok)) {
        if (sr1 != -1 && sr2 != -1) {
            addBType(op, func3, sr1, sr2);
            addLink(in, tok, pos, BRANCH);
        }
    }
    return pos;
}

u32 addSimpleBType(input &in, u32 op, u32 func3) {
    s32 sr1 = getRegister(in);
    s32 sr2 = getRegister(in);
    return addSimpleBType(in, op, func3, sr1, sr2);
}

u32 addReverseBType(input &in, u32 op, u32 func3) {
    s32 sr2 = getRegister(in);
    s32 sr1 = getRegister(in);
    return addSimpleBType(in, op, func3, sr1, sr2);
}

u32 addSimpleLoad(input &in, u32 op, u32 size) {
    alignTo(4);
    u32 pos = outpos;
    s32 rd = getRegister(in);
    if (rd == -1) return pos;

    token tok = getToken(in);
    if (!tok.base) {
        addError("Incomplete load", in);
        return pos;
    }

    s32 base = parseRegister(tok);
    if (base == -1) {
        // this is a labeled load
        if (validateLabel(in, tok)) {
            // auipc rd, symbol[31:12]
            // ld rd, rd, symbol[11:0]
            addUType(OP_AUIPC, rd, 0);
            addIType(op, size, rd, rd, 0);
            addLink(in, tok, pos, FULL_LOAD);
        }
    } else {
        token immt = getToken(in);
        if (immt.base) {
            u32 imm = readNumber(in, immt, 0xFFF);
            addIType(op, size, rd, base, imm);
        } else {
            addIType(op, size, rd, base, 0);
        }
    }

    return pos;
}

u32 addSimpleStore(input &in, u32 op, u32 size) {
    alignTo(4);
    u32 pos = outpos;
    s32 rs2 = getRegister(in);
    if (rs2 == -1) return pos;

    token tok = getToken(in);
    if (!tok.base) {
        addError("Incomplete store", in);
        return pos;
    }

    s32 base = parseRegister(tok);
    if (base == -1) {
        // this is a labeled store
        if (validateLabel(in, tok)) {
            s32 temp = getRegister(in);
            if (temp != -1) {
                // auipc tmp, symbol[31:12]
                // st rs2, tmp, symbol[11:0]
                addUType(OP_AUIPC, temp, 0);
                addRType(op, size, 0, 0, temp, rs2);
                addLink(in, tok, pos, FULL_STORE);
            }
        }
    } else {
        token immt = getToken(in);
        if (immt.base) {
            u32 imm = readNumber(in, immt, 0xFFF);
            addRType(op, size, (imm & 0xFE0) >> 5, imm & 0x1F, base, rs2);
        } else {
            addRType(op, size, 0, 0, base, rs2);
        }
    }

    return pos;
}

u32 addCSR(u32 op, u32 f3, u32 rd, u32 csr, u32 rs1) {
    alignTo(4);
    u32 pos = outpos;
    if (rd != -1 && rs1 != -1 && csr != BAD_CSR) {
        pos = addIType(op, f3, rd, rs1, csr);
    }
    return pos;
}

u32 addCSR(input &in, u32 op, u32 f3) {
    u32 rd = getRegister(in);
    u32 csr = getCSR(in);
    u32 rs1 = getRegister(in);
    return addCSR(op, f3, rd, csr, rs1);
}

u32 addCSRI(u32 op, u32 f3, u32 rd, u32 csr, input &in) {
    alignTo(4);
    u32 pos = outpos;
    token tok = getToken(in);
    if (!tok.base) {
        addError("No immediate specified for CSRxI", in);
        return pos;
    }

    u32 rs1 = readNumber(in, tok, 0x0000001F);
    rs1 = rs1 & 0x0000001F;

    if (rd != -1 && rs1 != -1 && csr != BAD_CSR) {
        pos = addIType(op, f3, rd, rs1, csr);
    }
    return pos;
}

u32 addCSRI(input &in, u32 op, u32 f3) {
    u32 rd = getRegister(in);
    u32 csr = getCSR(in);
    return addCSRI(op, f3, rd, csr, in);
}

u32 addLI(input &in) {
    alignTo(4);
    u32 pos = outpos;

    u32 reg = getRegister(in);
    if (reg == -1) return pos;

    token tok = getToken(in);
    if (!tok.base) {
        addError("Missing immediate for li", in);
        return pos;
    }

    u32 num = readNumber(in, tok, 0xFFFFFFFF);

    if ((num & 0xFFFFF000) == num) {
        addUType(OP_LUI, reg, num);
        return pos;
    }

    // add 0x00000800 so the sign extension on the second instruction
    // works in our favor.  I haven't proven it correct but I tested all
    // 32-bit values and it works.  When all else fails, resort to
    // exhaustive testing!
    addUType(OP_LUI, reg, (num + 0x00000800) & 0xFFFFF000);
    addIType(OP_IMM, F3_ADD, reg, reg, num & 0xFFF);

    return pos;
}

u32 addLA(input &in) {
    alignTo(4);
    u32 pos = outpos;

    u32 reg = getRegister(in);
    if (reg == -1) return pos;

    token tok = getToken(in);
    if (!tok.base) {
        addError("Missing label", in);
        return pos;
    }

    if (validateLabel(in, tok)) {
        addUType(OP_AUIPC, reg, 0);
        addIType(OP_IMM, F3_ADD, reg, reg, 0);
        addLink(in, tok, pos, IMM_LA);
    }

    return pos;
}


// ------------------------------- Instruction Parsing ------------------------------------

u32 parseInstruction(token base, input &in, bool &found) {
    found = false;

    #define ISWITCH() if (false)
    #define ICASE(str) } else if (is(base, str)) { found = true;
    #define IDEFAULT() } else {

    ISWITCH() {
    ICASE(".orig")
        u32 pos = outpos;
        token tok = getToken(in);
        if (!tok.base) {
            addError("Must specify an address for .orig", in);
        } else {
            u32 num = readNumber(in, tok, 0xFFFF);
            if (num < outpos) {
                addError(".orig can't go backwards", in, tok);
            } else {
                moveTo(num);
                pos = outpos;
            }
        }
        return pos;

    ICASE(".fillw")
        alignTo(4);
        u32 pos = outpos;
        u32 count = 0;
        while (true) {
            token tok = getToken(in);
            if (!tok.base) break;
            u32 num = readNumber(in, tok, 0xFFFFFFFF);
            addWord(num);
            count++;
        }
        if (!count) {
            addError("No bytes specified for .fillw", in);
        }
        return pos;

    ICASE(".fillh")
        alignTo(2);
        u32 pos = outpos;
        u32 count = 0;
        while (true) {
            token tok = getToken(in);
            if (!tok.base) break;
            u32 num = readNumber(in, tok, 0xFFFF);
            addHalf(num);
            count++;
        }
        if (!count) {
            addError("No bytes specified for .fillh", in);
        }
        return pos;

    ICASE(".fillb")
        u32 pos = outpos;
        u32 count = 0;
        while (true) {
            token tok = getToken(in);
            if (!tok.base) break;
            u32 num = readNumber(in, tok, 0xFF);
            addByte(num);
            count++;
        }
        if (!count) {
            addError("No bytes specified for .fillb", in);
        }
        return pos;

    ICASE(".filla")
        u32 pos = outpos;
        token tok = getToken(in);
        if (!tok.base) {
            addError("Must specify label for .filla", in);
        } else {
            if (validateLabel(in, tok)) {
                alignTo(4);
                pos = outpos;
                addWord(0);
                addLink(in, tok, pos, RAW_WORD);
            }
        }
        return pos;

    ICASE(".blkw")
        alignTo(4);
        u32 pos = outpos;
        token tok = getToken(in);
        if (!tok.base) {
            addError("Must specify word count for .blkw", in);
        } else {
            u32 num = readNumber(in, tok, 0xFFFFFFFF);
            advanceBy(num*4);
        }
        return pos;

    ICASE(".blkh")
        alignTo(2);
        u32 pos = outpos;
        token tok = getToken(in);
        if (!tok.base) {
            addError("Must specify half-word count for .blkh", in);
        } else {
            u32 num = readNumber(in, tok, 0xFFFFFFFF);
            advanceBy(num*2);
        }
        return pos;

    ICASE(".blkb")
        u32 pos = outpos;
        token tok = getToken(in);
        if (!tok.base) {
            addError("Must specify byte count for .blkb", in);
        } else {
            u32 num = readNumber(in, tok, 0xFF);
            advanceBy(num);
        }
        return pos;

    ICASE(".stringz")
        u32 pos = outpos;
        bool moreInLine = skipWhitespace(in);
        if (!moreInLine) {
            addError("Must specify string constant for .stringz", in);
        } else {
            addStringLit(in);
        }
        return pos;

    // R-Types
    ICASE("add")  return addSimpleRType(in, OP, F3_ADD , F7_NORM);
    ICASE("sub")  return addSimpleRType(in, OP, F3_ADD , F7_REV );
    ICASE("sll")  return addSimpleRType(in, OP, F3_SLL , F7_NORM);
    ICASE("slt")  return addSimpleRType(in, OP, F3_SLT , F7_NORM);
    ICASE("sltu") return addSimpleRType(in, OP, F3_SLTU, F7_NORM);
    ICASE("xor")  return addSimpleRType(in, OP, F3_XOR , F7_NORM);
    ICASE("srl")  return addSimpleRType(in, OP, F3_SRL , F7_NORM);
    ICASE("sra")  return addSimpleRType(in, OP, F3_SRL , F7_REV );
    ICASE("or")   return addSimpleRType(in, OP, F3_OR  , F7_NORM);
    ICASE("and")  return addSimpleRType(in, OP, F3_AND , F7_NORM);

    // Immediate I-Types
    ICASE("addi")  return addSimpleIType(in, OP_IMM, F3_ADD );
    ICASE("slti")  return addSimpleIType(in, OP_IMM, F3_SLT );
    ICASE("sltiu") return addSimpleIType(in, OP_IMM, F3_SLTU);
    ICASE("xori")  return addSimpleIType(in, OP_IMM, F3_XOR );
    ICASE("ori")   return addSimpleIType(in, OP_IMM, F3_OR  );
    ICASE("andi")  return addSimpleIType(in, OP_IMM, F3_AND );

    ICASE("slli")  return addShiftIType(in, OP_IMM, F3_SLL, F7_NORM);
    ICASE("srli")  return addShiftIType(in, OP_IMM, F3_SRL, F7_NORM);
    ICASE("srai")  return addShiftIType(in, OP_IMM, F3_SRL, F7_REV );

    // B-Types
    ICASE("beq")  return addSimpleBType(in, OP_BRANCH, F3_BEQ );
    ICASE("bne")  return addSimpleBType(in, OP_BRANCH, F3_BNE );
    ICASE("blt")  return addSimpleBType(in, OP_BRANCH, F3_BLT );
    ICASE("bge")  return addSimpleBType(in, OP_BRANCH, F3_BGE );
    ICASE("bltu") return addSimpleBType(in, OP_BRANCH, F3_BLTU);
    ICASE("bgeu") return addSimpleBType(in, OP_BRANCH, F3_BGEU);

    ICASE("jal")
        alignTo(4);
        u32 pos = outpos;

        token label = getToken(in);
        if (!label.base) {
            addError("JAL requires a jump target", in);
            return pos;
        }

        s32 link = parseRegister(label);
        if (link != -1) {
            label = getToken(in);
            if (!label.base) {
                addError("JAL w/ reg requires a jump target", in);
                return pos;
            }
        } else {
            link = 1; // implicit link register is x1
        }

        if (validateLabel(in, label)) {
            addUType(OP_JAL, link, 0);
            addLink(in, label, pos, IMM_JAL);
        }

        return pos;

    ICASE("j")
        alignTo(4);
        u32 pos = outpos;
        token label = getToken(in);
        if (validateLabel(in, label)) {
            addUType(OP_JAL, 0, 0);
            addLink(in, label, pos, IMM_JAL);
        }
        return pos;

    ICASE("jalr")
        alignTo(4);
        u32 pos = outpos;

        token t0 = getToken(in);
        token t1 = getToken(in);
        token t2 = getToken(in);

        if (!t0.base) {
            addError("JALR requires at least one param", in);
            return pos;
        }

        s32 link = 1;
        if (t2.base) {
            link = parseRegister(t0);
            if (link == -1) {
                addError("Invalid link register", in, t0);
                return pos;
            }

            t0 = t1;
            t1 = t2;
        }

        s32 target = parseRegister(t0);
        if (target == -1) {
            addError("Invalid target register", in, t0);
            return pos;
        }

        u32 offset = 0;
        if (t1.base) {
            offset = readNumber(in, t1, 0xFFF);
        }
        addIType(OP_JALR, 0, link, target, offset);

        return pos;

    ICASE("jr")
        alignTo(4);
        u32 pos = outpos;
        s32 link = 0;
        s32 target = getRegister(in);
        token tok = getToken(in);
        if (tok.base && target != -1) {
            u32 offset = readNumber(in, tok, 0xFFF);
            addIType(OP_JALR, 0, link, target, offset);
        }
        return pos;

    ICASE("lui")
        alignTo(4);
        u32 pos = outpos;

        s32 reg = getRegister(in);
        token tok = getToken(in);
        if (tok.base && reg != -1) {
            u32 upper = readNumber(in, tok, 0xFFFFF000);
            addUType(OP_LUI, reg, upper);
        }

        return pos;

    ICASE("auipc")
        alignTo(4);
        u32 pos = outpos;

        s32 reg = getRegister(in);
        token tok = getToken(in);
        if (tok.base && reg != -1) {
            u32 upper = readNumber(in, tok, 0xFFFFF000);
            addUType(OP_AUIPC, reg, upper);
        }

        return pos;

    ICASE("call")
        alignTo(4);
        u32 pos = outpos;
        token tok = getToken(in);
        if (validateLabel(in, tok)) {
            addUType(OP_AUIPC, 6, 0);
            addIType(OP_JALR, 0, 1, 6, 0);
            addLink(in, tok, pos, IMM_CALL);
        }
        return pos;

    ICASE("tail")
        alignTo(4);
        u32 pos = outpos;
        token tok = getToken(in);
        if (validateLabel(in, tok)) {
            addUType(OP_AUIPC, 6, 0);
            addIType(OP_JALR, 0, 0, 6, 0);
            addLink(in, tok, pos, IMM_CALL);
        }
        return pos;

    ICASE("lb")  return addSimpleLoad(in, OP_L, SIZE_B );
    ICASE("lh")  return addSimpleLoad(in, OP_L, SIZE_H );
    ICASE("lw")  return addSimpleLoad(in, OP_L, SIZE_W );
    ICASE("lbu") return addSimpleLoad(in, OP_L, SIZE_BU);
    ICASE("lhu") return addSimpleLoad(in, OP_L, SIZE_HU);

    ICASE("sb")  return addSimpleStore(in, OP_S, SIZE_B);
    ICASE("sh")  return addSimpleStore(in, OP_S, SIZE_H);
    ICASE("sw")  return addSimpleStore(in, OP_S, SIZE_W);

    ICASE("fence")   return addAlignedWord(FENCE_ALL);
    ICASE("fence.i") return addAlignedWord(FENCE_I);
    ICASE("ecall")   return addAlignedWord(ECALL);
    ICASE("ebreak")  return addAlignedWord(EBREAK);

    ICASE("csrrw") return addCSR(in, OP_SYSTEM, F3_RW);
    ICASE("csrrs") return addCSR(in, OP_SYSTEM, F3_RS);
    ICASE("csrrc") return addCSR(in, OP_SYSTEM, F3_RC);
    ICASE("csrrwi") return addCSRI(in, OP_SYSTEM, F3_RWI);
    ICASE("csrrsi") return addCSRI(in, OP_SYSTEM, F3_RSI);
    ICASE("csrrci") return addCSRI(in, OP_SYSTEM, F3_RCI);

    // Pseudo-Instructions
    ICASE("nop") return addAlignedWord(NOP);
    ICASE("ret") return addAlignedWord(RET);

    ICASE("li") return addLI(in);
    ICASE("la") return addLA(in);

    ICASE("mv")   return addSimpleIType(in, OP_IMM, F3_ADD, getRegister(in), getRegister(in), 0);
    ICASE("not")  return addSimpleIType(in, OP_IMM, F3_XOR, getRegister(in), getRegister(in), -1);
    ICASE("neg")  return addSimpleRType(in, OP, F3_ADD, F7_REV, getRegister(in), 0, getRegister(in));
    ICASE("seqz") return addSimpleIType(in, OP_IMM, F3_SLTU, getRegister(in), getRegister(in), 1);
    ICASE("snez") return addSimpleRType(in, OP, F3_SLTU, F7_NORM, getRegister(in), 0, getRegister(in));
    ICASE("sltz") return addSimpleRType(in, OP, F3_SLT, F7_NORM, getRegister(in), getRegister(in), 0);
    ICASE("sgtz") return addSimpleRType(in, OP, F3_SLT, F7_NORM, getRegister(in), 0, getRegister(in));

    ICASE("beqz") return addSimpleBType(in, OP_BRANCH, F3_BEQ, getRegister(in), 0);
    ICASE("bnez") return addSimpleBType(in, OP_BRANCH, F3_BNE, getRegister(in), 0);
    ICASE("blez") return addSimpleBType(in, OP_BRANCH, F3_BGE, 0, getRegister(in));
    ICASE("bgez") return addSimpleBType(in, OP_BRANCH, F3_BGE, getRegister(in), 0);
    ICASE("bltz") return addSimpleBType(in, OP_BRANCH, F3_BLT, getRegister(in), 0);
    ICASE("bgtz") return addSimpleBType(in, OP_BRANCH, F3_BLT, 0, getRegister(in));
    ICASE("bgt")  return addReverseBType(in, OP_BRANCH, F3_BLT);
    ICASE("ble")  return addReverseBType(in, OP_BRANCH, F3_BGE);
    ICASE("bgtu") return addReverseBType(in, OP_BRANCH, F3_BLTU);
    ICASE("bleu") return addReverseBType(in, OP_BRANCH, F3_BGEU);

    ICASE("rdinstret")  return addCSR(OP_SYSTEM, F3_RS, getRegister(in), CSR_INSTRET , 0);
    ICASE("rdinstreth") return addCSR(OP_SYSTEM, F3_RS, getRegister(in), CSR_INSTRETH, 0);
    ICASE("rdcycle")    return addCSR(OP_SYSTEM, F3_RS, getRegister(in), CSR_CYCLE   , 0);
    ICASE("rdcycleh")   return addCSR(OP_SYSTEM, F3_RS, getRegister(in), CSR_CYCLEH  , 0);
    ICASE("rdtime")     return addCSR(OP_SYSTEM, F3_RS, getRegister(in), CSR_TIME    , 0);
    ICASE("rdtimeh")    return addCSR(OP_SYSTEM, F3_RS, getRegister(in), CSR_TIMEH   , 0);

    ICASE("csrr")  return addCSR(OP_SYSTEM, F3_RS, getRegister(in), getCSR(in), 0);
    ICASE("csrw")  return addCSR(OP_SYSTEM, F3_RW, 0, getCSR(in), getRegister(in));
    ICASE("csrs")  return addCSR(OP_SYSTEM, F3_RS, 0, getCSR(in), getRegister(in));
    ICASE("csrc")  return addCSR(OP_SYSTEM, F3_RC, 0, getCSR(in), getRegister(in));

    ICASE("csrwi") return addCSRI(OP_SYSTEM, F3_RW, 0, getCSR(in), in);
    ICASE("csrsi") return addCSRI(OP_SYSTEM, F3_RS, 0, getCSR(in), in);
    ICASE("csrci") return addCSRI(OP_SYSTEM, F3_RC, 0, getCSR(in), in);

    IDEFAULT()
        return outpos;
    }

    #undef ISWITCH
    #undef ICASE
    #undef IDEFAULT
}


// ------------------------------- Top-Level Flow ------------------------------------

void readLine(input &in) {
    token first = getToken(in);
    if (first.base) {
        bool unlabeled;
        u32 offset = parseInstruction(first, in, unlabeled);
        if (!unlabeled) {
            if (validateLabel(in, first)) {
                addLabel(in, first);
            }
            
            token next = getToken(in);
            if (next.base) {
                bool success;
                offset = parseInstruction(next, in, success);
                if (!success) {
                    addError("Unknown token", in, next);
                    while (true) {
                        token t = getToken(in);
                        if (!t.base) break;
                    }
                } else {
                    processLabels(offset);
                }
            }
        } else {
            processLabels(offset);
        }
    }

    finishEmptyLine(in);
    // skip newline and go to next line
    if (in.position < in.end) {
        in.position++;
        in.line++;
    }
}

void readProgram(u8 *buffer, u32 len) {
    input in;
    in.line = 1;
    in.base = buffer;
    in.position = buffer;
    in.end = buffer + len;

    while (in.position < in.end) {
        readLine(in);
    }
}

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: assemble <programfile>\n");
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

    u8 *buffer = new u8[length];

    if (!buffer) {
        printf("Couldn't allocate %llu bytes for file %s\n", length, argv[1]);
        exit(EXIT_BAD_MALLOC);
    }

    fread(buffer, 1, length, f);
    fclose(f);

    readProgram(buffer, (u32) length);
    processLabels(outpos);

    resolveLinks();

    if (errorCount > 0) {
        printf("Assembly failed with %d errors\n", errorCount);
        exit(EXIT_ERRORS);
    }

    char outFile[258];
    strncpy(outFile, argv[1], 256);
    outFile[256] = '\0';
    u32 pos = strlen(outFile) - 1;
    if (pos > 255) { // overflow
        strcpy(outFile, "out.o");
    } else {
        while (true) {
            char curr = outFile[pos];
            if (curr == '\\' || curr == '/' || pos == 0) {
                strcpy(outFile, "out.o");
                break;
            }
            if (curr == '.') {
                strcpy(outFile + pos + 1, "o");
                break;
            }
            pos--;
        }
    }

    printf("Saving result to %s\n", outFile);

    FILE *out = fopen(outFile, "wb");
    if (!out) {
        printf("Couldn't open %s for writing\n", outFile);
        exit(EXIT_BAD_FILE);
    }

    fwrite(output, 1, outpos, out);
    fclose(out);

    exit(0);
    
    delete [] buffer;
}

