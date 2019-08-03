struct Instruction {
    opcode: Opcode,
    operands: Operands
}

enum Opcode {
    DADD
    DADDI
    DADDIU
    DADDU
    DSUB
    DSUBU
    DMULT
    DMULTU
    DDIV
    DDIVU

    ADD
    ADDU
    ADDI
    ADDIU
    SUB
    SUBU
    MULT
    MULTU
    DIV
    DIVU

    NOR
    AND
    OR
    XOR
    ANDI
    ORI
    XORI

    BEQ
    BEQL
    BGEZ
    BGEZAL
    BGEZALL
    BGEZL
    BGTZ
    BGTZL
    BLEZ
    BLEZL
    BLTZ
    BLTZAL
    BLTZALL
    BLTZL
    BNE
    BNEL
    BREAK
    COPz
    DSLL
    DSLL32
    DSLLV
    DSRA
    DSRA32
    DSRAV
    DSRL
    DSRL32
    DSRLV
    J
    JAL
    JALR
    JR
    LB
    LBU
    LD
    LDCz
    LDL
    LDR
    LH
    LHU
    LL
    LLD
    LUI
    LW
    LWCz
    LWL
    LWR
    LWU
    MFHI
    MFLO
    MOVN
    MOVZ
    MTHI
    MTLO
    PREF
    REGIMM
    SB
    SC
    SCD
    SD
    SDCz
    SDL
    SDR
    SH
    SLL
    SLLV
    SLT
    SLTI
    SLTII
    SRA
    SRAV
    SRL
    SRLV
    SW
    SWCz
    SWL
    SWR
    SYNC
    SYSCALL

    TEQ
    TGE
    TGEU
    TLT
    TLTU
    TNE
    TEQI
    TGEI
    TGEIU
    TLTI
    TLTIU
    TNEI
}

struct RRR(u32);
struct RImm(u32);
struct RRImm(u32);

enum Operands {
    RRR(RRR),
    RImm(RImm),
    RRImm(RRImm),
    CoProc(u32),
    Code(u32), // 20-bit number for BREAK or SYSCALL
    Offset(u32), // J or JAL
}

// ADD
// SPECIAL  rs     rt     rd     0      ADD
// 000000   XXXXX  XXXXX  XXXXX  00000  100000
//
// ADDU
// SPECIAL  rs     rt     rd     0      ADDU
// 000000   XXXXX  XXXXX  XXXXX  00000  100001
//
// AND
// SPECIAL  rs     rt     rd     0      ADDU
// 000000   XXXXX  XXXXX  XXXXX  00000  100100
//
// ADDI     rs     rt     imm16
// 001000   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// ADDIU    rs     rt     imm16
// 001001   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// ANDI     rs     rt     imm16
// 001100   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// BEQ      rs     rt     imm16
// 000100   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// BEQL     rs     rt     imm16
// 010100   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// BGEZ
// REGIMM   rs     BGEZ   imm16
// 000001   XXXXX  00001  YYYYYYYYYYYYYYYY
//
// BGEZAL
// REGIMM   rs     BGEZAL imm16
// 000001   XXXXX  10001  YYYYYYYYYYYYYYYY
//
// BGEZALL
// REGIMM   rs     BGEZALLimm16
// 000001   XXXXX  10011  YYYYYYYYYYYYYYYY
//
// BGEZL
// REGIMM   rs     BGEZL  imm16
// 000001   XXXXX  00011  YYYYYYYYYYYYYYYY
//
// BGTZ     rs     0      imm16
// 000111   XXXXX  00000  YYYYYYYYYYYYYYYY
//
// BGTZL    rs     0      imm16
// 010111   XXXXX  00000  YYYYYYYYYYYYYYYY
//
// BLEZ     rs     0      imm16
// 000110   XXXXX  00000  YYYYYYYYYYYYYYYY
//
// BLEZL    rs     0      imm16
// 010110   XXXXX  00000  YYYYYYYYYYYYYYYY
//
// BLTZ
// REGIMM   rs     BLTZ   imm16
// 000001   XXXXX  00000  YYYYYYYYYYYYYYYY
//
// BLTZAL
// REGIMM   rs     BLTZAL imm16
// 000001   XXXXX  10000  YYYYYYYYYYYYYYYY
//
// BLTZALL
// REGIMM   rs     BLTZALLimm16
// 000001   XXXXX  10010  YYYYYYYYYYYYYYYY
//
// BLTZL
// REGIMM   rs     BLTZL  imm16
// 000001   XXXXX  00010  YYYYYYYYYYYYYYYY
//
// BNE      rs     rt     imm16
// 000101   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// BNEL     rs     rt     imm16
// 010101   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// BREAK
// SPECIAL  code                  BREAK
// 000000   XXXXXXXXXXXXXXXXXXXX  001101
//
// COPz     cop_fun
// 0100zz   XXXXXXXXXXXXXXXXXXXXXXXXXX
//
// DADD
// SPECIAL  rs     rt     rd     0      DADD
// 000000   XXXXX  XXXXX  XXXXX  00000  101100
//
// DADDU
// SPECIAL  rs     rt     rd     0      DADDU
// 000000   XXXXX  XXXXX  XXXXX  00000  101101
//
// DADDI    rs     rt     imm16
// 011000   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// DADDIU   rs     rt     imm16
// 011001   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// DSUB
// SPECIAL  rs     rt     rd     0      DSUB
// 000000   XXXXX  XXXXX  XXXXX  00000  101110
//
// DSUBU
// SPECIAL  rs     rt     rd     0      DSUBU
// 000000   XXXXX  XXXXX  XXXXX  00000  101111
//
// DIV
// SPECIAL  rs     rt     0           DIV
// 000000   XXXXX  XXXXX  0000000000  011010
//
// DIVU
// SPECIAL  rs     rt     0           DIVU
// 000000   XXXXX  XXXXX  0000000000  011011
//
// DDIV
// SPECIAL  rs     rt     0           DDIV
// 000000   XXXXX  XXXXX  0000000000  011110
//
// DDIVU
// SPECIAL  rs     rt     0           DDIVU
// 000000   XXXXX  XXXXX  0000000000  011111
//
// DMULT
// SPECIAL  rs     rt     0           DMULT
// 000000   XXXXX  XXXXX  0000000000  011100
//
// DMULTU
// SPECIAL  rs     rt     0           DMULTU
// 000000   XXXXX  XXXXX  0000000000  011101
//
// DSLL
// SPECIAL  0      rt     rd     sa     DSLL
// 000000   00000  XXXXX  XXXXX  XXXXX  111000
//
// DSLL32
// SPECIAL  0      rt     rd     sa     DSLL32
// 000000   00000  XXXXX  XXXXX  XXXXX  111100
//
// DSLLV
// SPECIAL  rs     rt     rd     0      DSLLV
// 000000   XXXXX  XXXXX  XXXXX  00000  010100
//
// DSRA
// SPECIAL  0      rt     rd     sa     DSRA
// 000000   00000  XXXXX  XXXXX  XXXXX  111011
//
// DSRA32
// SPECIAL  0      rt     rd     sa     DSRA32
// 000000   00000  XXXXX  XXXXX  XXXXX  111111
//
// DSRAV
// SPECIAL  rs     rt     rd     0      DSRAV
// 000000   XXXXX  XXXXX  XXXXX  00000  010111
//
// DSRL
// SPECIAL  0      rt     rd     sa     DSRL
// 000000   00000  XXXXX  XXXXX  XXXXX  111010
//
// DSRL32
// SPECIAL  0      rt     rd     sa     DSRL32
// 000000   00000  XXXXX  XXXXX  XXXXX  111110
//
// DSRLV
// SPECIAL  rs     rt     rd     0      DSRLV
// 000000   XXXXX  XXXXX  XXXXX  00000  010110
//
// J        instr_index
// 000010   XXXXXXXXXXXXXXXXXXXXXXXXXX
//
// JAL      instr_index
// 000011   XXXXXXXXXXXXXXXXXXXXXXXXXX
//
// JALR            v-------------v---  reserved? might not be? unknown
// SPECIAL  rs            rd            JALR
// 000000   XXXXX  00000  XXXXX  00000  001001
//
// JR              v-----v---  reserved? might not be? unknown
// SPECIAL  rs                      JR
// 000000   XXXXX  000000000000000  001000
//
// LB       base   rt     offset
// 100000   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LBU      base   rt     offset
// 100100   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LD       base   rt     offset
// 110111   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LDCz     base   rt     offset
// 1101zz   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LDL      base   rt     offset
// 011010   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LDR      base   rt     offset
// 011011   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LH       base   rt     offset
// 100001   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LHU      base   rt     offset
// 100101   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LL       base   rt     offset
// 110000   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LLD      base   rt     offset
// 110100   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LUI      0      rt     imm16
// 001111   00000  XXXXX  XXXXXXXXXXXXXXXX
//
// LW       base   rt     offset
// 100011   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LWCz     base   rt     offset
// 1100zz   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LWL      base   rt     offset
// 100010   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LWR      base   rt     offset
// 100110   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LWU      base   rt     offset
// 100111   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// MFHI
// SPECIAL  0           rd     0      MFHI
// 000000   0000000000  XXXXX  00000  010000
//
// MFLO
// SPECIAL  0           rd     0      MFLO
// 000000   0000000000  XXXXX  00000  010010
//
// MOVN
// SPECIAL  rs     rt     rd     0      MOVN
// 000000   XXXXX  XXXXX  XXXXX  00000  001011
//
// MOVZ
// SPECIAL  rs     rt     rd     0      MOVZ
// 000000   XXXXX  XXXXX  XXXXX  00000  001010
//
// MTHI
// SPECIAL  rs     0                MTHI
// 000000   XXXXX  000000000000000  010001
//
// MTLO
// SPECIAL  rs     0                MTLO
// 000000   XXXXX  000000000000000  010011
//
// MULT
// SPECIAL  rs     rt     0           MULT
// 000000   XXXXX  XXXXX  0000000000  011000
//
// MULTU
// SPECIAL  rs     rt     0           MULTU
// 000000   XXXXX  XXXXX  0000000000  011001
//
// NOR
// SPECIAL  rs     rt     rd     0      NOR
// 000000   XXXXX  XXXXX  XXXXX  00000  100111
//
// OR
// SPECIAL  rs     rt     rd     0      OR
// 000000   XXXXX  XXXXX  XXXXX  00000  100101
//
// ORI      rs     rt     imm16
// 001101   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// PREF     base   hint   imm16
// 1100111  XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// SB       base   rt     imm16
// 101000   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// SC       base   rt     imm16
// 111000   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// SCD      base   rt     imm16
// 111100   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// SD       base   rt     imm16
// 111111   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// SDCz     base   rt     imm16
// 1111zz   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// SDL      base   rt     imm16
// 101100   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// SDR      base   rt     imm16
// 101101   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// SH       base   rt     imm16
// 101001   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// SLL
// SPECIAL  0      rt     rd    sa    SLL
// 000000   00000  XXXXX  XXXXX XXXXX 000000
//
// SLLV
// SPECIAL  rs     rt     rd    0     SLLV
// 000000   XXXXX  XXXXX  XXXXX 00000 000100
//
// SLT
// SPECIAL  rs     rt     rd    0     SLT
// 000000   XXXXX  XXXXX  XXXXX 00000 101010
//
// SLTI     rs     rt     imm16
// 001010   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// SLTII    rs     rt     imm16
// 001011   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// SRA
// SPECIAL  0      rt     rd     sa     SRA
// 000000   00000  XXXXX  XXXXX  XXXXX  000011
//
// SRAV
// SPECIAL  rs     rt     rd     0      SRAV
// 000000   XXXXX  XXXXX  XXXXX  00000  000111
//
// SRL
// SPECIAL  0      rt     rd     sa     SRL
// 000000   00000  XXXXX  XXXXX  XXXXX  000010
//
// SRLV
// SPECIAL  rs     rt     rd     0      SRLV
// 000000   XXXXX  XXXXX  XXXXX  00000  000110
//
// SUB
// SPECIAL  rs     rt     rd     0      SUB
// 000000   XXXXX  XXXXX  XXXXX  00000  100010
//
// SUBU
// SPECIAL  rs     rt     rd     0      SUBU
// 000000   XXXXX  XXXXX  XXXXX  00000  100011
//
// SW       base   rt     imm16
// 101011   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// SWCz     base   rt     imm16
// 1110zz   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// SWL      base   rt     imm16
// 101010   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// SWR      base   rt     imm16
// 101110   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// SYNC
// SPECIAL  0                stype  SYNC
// 000000   000000000000000  XXXXX  001111
//
// SYSCALL
// SPECIAL  Code                  SYSCALL
// 000000   XXXXXXXXXXXXXXXXXXXX  001100
//
// TEQ
// SPECIAL  rs     rt     code        TEQ
// 000000   XXXXX  XXXXX  0000000000  110100
//
// TEQI
// REGIMM   rs     TEQI   imm16
// 000001   XXXXX  01100  XXXXXXXXXXXXXXXX
//
// TGE
// SPECIAL  rs     rt     code        TGE
// 000000   XXXXX  XXXXX  0000000000  110000
//
// TGEI
// REGIMM   rs     TGEI   imm16
// 000001   XXXXX  01000  XXXXXXXXXXXXXXXX
//
// TGEU
// SPECIAL  rs     rt     code        TGEU
// 000000   XXXXX  XXXXX  0000000000  110001
//
// TGEIU
// REGIMM   rs     TGEIU  imm16
// 000001   XXXXX  01001  XXXXXXXXXXXXXXXX
//
// TLT
// SPECIAL  rs     rt     code        TLT
// 000000   XXXXX  XXXXX  0000000000  110010
//
// TLTI
// REGIMM   rs     TLTI   imm16
// 000001   XXXXX  01010  XXXXXXXXXXXXXXXX
//
// TLTU
// SPECIAL  rs     rt     code        TLTU
// 000000   XXXXX  XXXXX  0000000000  110011
//
// TLTIU
// REGIMM   rs     TLTIU  imm16
// 000001   XXXXX  01011  XXXXXXXXXXXXXXXX
//
// TNE
// SPECIAL  rs     rt     code        TNE
// 000000   XXXXX  XXXXX  0000000000  110110
//
// TNEI
// REGIMM   rs     TNEI   imm16
// 000001   XXXXX  01110  XXXXXXXXXXXXXXXX
//
// XOR
// SPECIAL  rs     rt     rd     0      XOR
// 000000   XXXXX  XXXXX  XXXXX  00000  100110
//
// XORI     rs     rt     imm16
// 001110   XXXXX  XXXXX  XXXXXXXXXXXXXXXX





