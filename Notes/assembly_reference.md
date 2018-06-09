## meta-instructions:

.orig <addr>
    start describing memory at this address

.blkw <count>
    reserve <count> words of memory

.blkh <count>
    reserve <count> half-words of memory

.blkb <count>
    reserve <count> bytes of memory

.fillw <value>[ <value>[ <value>...]]
    set a 32-bit constant

.filla <value>
    set a 32-bit address

.fillh <value>
    set a 16-bit value

.fillb <value>
    set a 8-bit value

.stringz "literal"
    set a zero-terminated string

## syntax:

[label]whitespace(meta-instruction|instruction|empty)[;comment]
