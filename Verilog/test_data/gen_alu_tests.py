import ctypes
import random

def s32(val):
    return ctypes.c_int(val).value

def u32(val):
    return val & 0xFFFFFFFF

def shift(val):
    return val & 0x1F

def test(a, b, op, rev, result):
    print "%X %X %X %X %X" % (u32(a), u32(b), u32(op), u32(rev), u32(result))

for c in range(20):
    a = s32(random.getrandbits(32))
    b = s32(random.getrandbits(32))
    add = s32(a + b)
    sub = s32(a - b)
    sll = s32(u32(a) << shift(b))
    slt = 1 if a < b else 0
    sltu = 1 if u32(a) < u32(b) else 0
    xor = s32(a ^ b)
    srl = s32(u32(a) >> shift(b))
    sra = s32(a >> shift(b))
    bor = s32(a | b)
    band = s32(a & b)
    test(a, b, 0, 0, add)
    test(a, b, 0, 1, sub)
    test(a, b, 1, 0, sll)
    test(a, b, 1, 1, sll)
    test(a, b, 2, 0, slt)
    test(a, b, 2, 1, slt)
    test(a, b, 3, 0, sltu)
    test(a, b, 3, 1, sltu)
    test(a, b, 4, 0, xor)
    test(a, b, 4, 1, xor)
    test(a, b, 5, 0, srl)
    test(a, b, 5, 1, sra)
    test(a, b, 6, 0, bor)
    test(a, b, 6, 1, bor)
    test(a, b, 7, 0, band)
    test(a, b, 7, 1, band)

