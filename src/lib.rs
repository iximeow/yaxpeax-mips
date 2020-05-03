extern crate num_enum;
extern crate yaxpeax_arch;

use std::convert::TryInto;
use std::fmt;
use std::mem;

use num_enum::IntoPrimitive;

use yaxpeax_arch::{Arch, AddressDiff, Decoder, LengthedInstruction};

mod display;

#[derive(Debug, PartialEq)]
pub enum DecodeError {
    ExhaustedInput,
    InvalidOpcode,
    InvalidOperand,
}

impl fmt::Display for DecodeError {
    fn fmt(&self, f:  &mut fmt::Formatter) -> fmt::Result {
        match self {
            DecodeError::ExhaustedInput => write!(f, "exhausted input"),
            DecodeError::InvalidOpcode => write!(f, "invalid opcode"),
            DecodeError::InvalidOperand => write!(f, "invalid operand"),
        }
    }
}

impl yaxpeax_arch::DecodeError for DecodeError {
    fn data_exhausted(&self) -> bool { self == &DecodeError::ExhaustedInput }
    fn bad_opcode(&self) -> bool { self == &DecodeError::InvalidOpcode }
    fn bad_operand(&self) -> bool { self == &DecodeError::InvalidOperand }
}


#[derive(Debug, PartialEq)]
pub struct Instruction {
    word: u32,
    operands: [OperandSpec; 3],
    opcode: Opcode,
}

impl Default for Instruction {
    fn default() -> Self {
        Instruction {
            word: 0,
            operands: [OperandSpec::Nothing, OperandSpec::Nothing, OperandSpec::Nothing],
            opcode: Opcode::J,
        }
    }
}

impl Instruction {
    fn operand(&self, op: &OperandSpec) -> Option<Operand> {
        match op {
            OperandSpec::Nothing => None,
            OperandSpec::Rs => { Some(Operand::Reg((self.word >> 21) as u8 & 0b11111)) },
            OperandSpec::Rt => { Some(Operand::Reg((self.word >> 16) as u8 & 0b11111)) },
            OperandSpec::Rd => { Some(Operand::Reg((self.word >> 11) as u8 & 0b11111)) },
            OperandSpec::Sa => { Some(Operand::Shift((self.word >> 6) as u8 & 0b11111)) },
            OperandSpec::Code => { Some(Operand::Imm(((self.word >> 6) as u16 & 0b11_1111_1111) as i16)) },
            OperandSpec::Syscall => { Some(Operand::LongImm((self.word >> 6) & 0xf_ff_ff)) },
            OperandSpec::Imm16 => { Some(Operand::Imm(self.word as u16 as i16)) },
            OperandSpec::BaseOffset => { Some(Operand::BaseOffset(
                ((self.word >> 21) & 0b11111) as u8,
                (self.word & 0xffff) as u16 as i16,
            )) },
            OperandSpec::Stype => {
                Some(Operand::Imm(((self.word >> 6) & 0b11111) as i16))
            },
            OperandSpec::JOffset => {
                Some(Operand::JOffset(((self.word as i32) << 6) >> 4))
            },
            OperandSpec::CoprocessorOpcode => {
                Some(Operand::LongImm(self.word & 0x03_ff_ff_ff))
            },
        }
    }
}

impl yaxpeax_arch::Instruction for Instruction {
    fn well_defined(&self) -> bool {
        // TODO: this is inaccurate
        true
    }
}

impl LengthedInstruction for Instruction {
    type Unit = AddressDiff<u32>;
    fn min_size() -> Self::Unit {
        AddressDiff::from_const(4)
    }

    fn len(&self) -> Self::Unit {
        AddressDiff::from_const(4)
    }
}

#[derive(Debug, IntoPrimitive, PartialEq)]
#[repr(u8)]
pub enum Opcode {
    J       = 0b00_000010,
    JAL     = 0b00_000011,
    BEQ     = 0b00_000100,
    BNE     = 0b00_000101,
    BLEZ    = 0b00_000110,
    BGTZ    = 0b00_000111,
    ADDI    = 0b00_001000,
    ADDIU   = 0b00_001001,
    SLTI    = 0b00_001010,
    SLTIU   = 0b00_001011,
    ANDI    = 0b00_001100,
    ORI     = 0b00_001101,
    XORI    = 0b00_001110,
    LUI     = 0b00_001111,
    COP1    = 0b00_010000,
    COP2    = 0b00_010001,
    COP3    = 0b00_010010,
    COP4    = 0b00_010011,
    BEQL    = 0b00_010100,
    BNEL    = 0b00_010101,
    BLEZL   = 0b00_010110,
    BGTZL   = 0b00_010111,
    DADDI   = 0b00_011000,
    DADDIU  = 0b00_011001,
    LDL     = 0b00_011010,
    LDR     = 0b00_011011,
    LB      = 0b00_100000,
    LH      = 0b00_100001,
    LWL     = 0b00_100010,
    LW      = 0b00_100011,
    LBU     = 0b00_100100,
    LHU     = 0b00_100101,
    LWR     = 0b00_100110,
    LWU     = 0b00_100111,
    SB      = 0b00_101000,
    SH      = 0b00_101001,
    SWL     = 0b00_101010,
    SW      = 0b00_101011,
    SDL     = 0b00_101100,
    SDR     = 0b00_101101,
    SWR     = 0b00_101110,
    LL      = 0b00_110000,
    LWC1    = 0b00_110001,
    LWC2    = 0b00_110010,
    PREF    = 0b00_110011,
    LLD     = 0b00_110100,
    LDC1    = 0b00_110101,
    LDC2    = 0b00_110110,
    LD      = 0b00_110111,
    SC      = 0b00_111000,
    SWC1    = 0b00_111001,
    SWC2    = 0b00_111010,
    SCD     = 0b00_111100,
    SDC1    = 0b00_111101,
    SDC2    = 0b00_111110,
    SD      = 0b00_111111,

    SLL     = 0b01_000000,
    SRL     = 0b01_000010,
    SRA     = 0b01_000011,
    SLLV    = 0b01_000100,
    SRLV    = 0b01_000110,
    SRAV    = 0b01_000111,
    JR      = 0b01_001000,
    JALR    = 0b01_001001,
    MOVZ    = 0b01_001010,
    MOVN    = 0b01_001011,
    SYSCALL = 0b01_001100,
    BREAK   = 0b01_001101,
    SYNC    = 0b01_001111,
    MFHI    = 0b01_010000,
    MTHI    = 0b01_010001,
    MFLO    = 0b01_010010,
    MTLO    = 0b01_010011,
    DSLLV   = 0b01_010100,
    DSRLV   = 0b01_010110,
    DSRAV   = 0b01_010111,
    MULT    = 0b01_011000,
    MULTU   = 0b01_011001,
    DIV     = 0b01_011010,
    DIVU    = 0b01_011011,
    DMULT   = 0b01_011100,
    DMULTU  = 0b01_011101,
    DDIV    = 0b01_011110,
    DDIVU   = 0b01_011111,
    ADD     = 0b01_100000,
    ADDU    = 0b01_100001,
    SUB     = 0b01_100010,
    SUBU    = 0b01_100011,
    AND     = 0b01_100100,
    OR      = 0b01_100101,
    XOR     = 0b01_100110,
    NOR     = 0b01_100111,
    SLT     = 0b01_101010,
    DADD    = 0b01_101100,
    DADDU   = 0b01_101101,
    DSUB    = 0b01_101110,
    DSUBU   = 0b01_101111,
    TGE     = 0b01_110000,
    TGEU    = 0b01_110001,
    TLT     = 0b01_110010,
    TLTU    = 0b01_110011,
    TEQ     = 0b01_110100,
    TNE     = 0b01_110110,
    DSLL    = 0b01_111000,
    DSRL    = 0b01_111010,
    DSRA    = 0b01_111011,
    DSLL32  = 0b01_111100,
    DSRL32  = 0b01_111110,
    DSRA32  = 0b01_111111,

    BLTZ    = 0b10_000000,
    BGEZ    = 0b10_000001,
    BLTZL   = 0b10_000010,
    BGEZL   = 0b10_000011,
    TGEI    = 0b10_001000,
    TGEIU   = 0b10_001001,
    TLTI    = 0b10_001010,
    TLTIU   = 0b10_001011,
    TEQI    = 0b10_001100,
    TNEI    = 0b10_001110,
    BLTZAL  = 0b10_010000,
    BGEZAL  = 0b10_010001,
    BLTZALL = 0b10_010010,
    BGEZALL = 0b10_010011,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum OperandSpec {
    Nothing = 0,
    Rs,
    Rt,
    Rd,
    Sa,
    Code,
    Syscall,
    Imm16,
    BaseOffset,
    Stype,
    JOffset,
    CoprocessorOpcode,
}

#[derive(Debug)]
pub enum Operand {
    Reg(u8),
    Imm(i16),
    BaseOffset(u8, i16),
    Shift(u8),
    LongImm(u32),
    JOffset(i32)
}

#[cfg(feature="use-serde")]
#[derive(Debug, Serialize, Deserialize)]
pub struct MIPS;

#[cfg(not(feature="use-serde"))]
#[derive(Debug)]
pub struct MIPS;

impl Arch for MIPS {
    type Address = u32;
    type Instruction = Instruction;
    type DecodeError = DecodeError;
    type Decoder = MipsDecoder;
    type Operand = Operand;
}

#[derive(Default, Debug)]
pub struct MipsDecoder {}

impl Decoder<Instruction> for MipsDecoder {
    type Error = DecodeError;

    fn decode_into<T: IntoIterator<Item=u8>>(&self, instruction: &mut Instruction, bytes: T) -> Result<(), Self::Error> {
        let mut bytes_iter = bytes.into_iter();
        let word: Vec<u8> = bytes_iter.by_ref().take(4).collect();
        let word = u32::from_le_bytes(word.as_slice().try_into().map_err(|_| DecodeError::ExhaustedInput)?);

        instruction.word = word;

        let opc = (word >> 26) as u8;

        const SPECIAL: u8 = 0b000000;
        const REG_IMM: u8 = 0b000001;

        if opc == SPECIAL {
            let opc = (word & 0b111111) as u8;

            if [0b000101, 0b001110, 0b010101, 0b101000, 0b101001, 0b110101, 0b110111, 0b111001, 0b111101].contains(&opc) {
                return Err(DecodeError::InvalidOpcode);
            }

            // operands in the secondary map have kind of a mess of operands. except for 0b100000
            // and above, which have nice patterns:
            if opc >= 0b010000 {
                instruction.operands = match (opc >> 4) - 1 {
                    0b000 => {
                        [OperandSpec::Rs, OperandSpec::Rt, OperandSpec::Nothing]
                    },
                    0b001 |
                    0b010 => {
                        [OperandSpec::Rd, OperandSpec::Rs, OperandSpec::Rt]
                    },
                    0b011 => {
                        [OperandSpec::Rs, OperandSpec::Rt, OperandSpec::Code]
                    },
                    0b100 => {
                        [OperandSpec::Rt, OperandSpec::Rd, OperandSpec::Sa]
                    }
                    _ => {
                        unreachable!("opc must be 0b011xxx or 0b1xxxx at this point");
                    }
                };
            } else {
                // just do a table lookup...
                instruction.operands = [
                    [OperandSpec::Rt, OperandSpec::Rd, OperandSpec::Sa],
                    [OperandSpec::Nothing, OperandSpec::Nothing, OperandSpec::Nothing],
                    [OperandSpec::Rt, OperandSpec::Rd, OperandSpec::Sa],
                    [OperandSpec::Rt, OperandSpec::Rd, OperandSpec::Sa],
                    [OperandSpec::Rs, OperandSpec::Rt, OperandSpec::Rd],
                    [OperandSpec::Nothing, OperandSpec::Nothing, OperandSpec::Nothing],
                    [OperandSpec::Rs, OperandSpec::Rt, OperandSpec::Rd],
                    [OperandSpec::Rs, OperandSpec::Rt, OperandSpec::Rd],
                    [OperandSpec::Rs, OperandSpec::Nothing, OperandSpec::Nothing],
                    [OperandSpec::Rs, OperandSpec::Rd, OperandSpec::Nothing],
                    [OperandSpec::Rs, OperandSpec::Rt, OperandSpec::Rd],
                    [OperandSpec::Rs, OperandSpec::Rt, OperandSpec::Rd],
                    [OperandSpec::Syscall, OperandSpec::Nothing, OperandSpec::Nothing],
                    [OperandSpec::Syscall, OperandSpec::Nothing, OperandSpec::Nothing],
                    [OperandSpec::Nothing, OperandSpec::Nothing, OperandSpec::Nothing],
                    [OperandSpec::Stype, OperandSpec::Nothing, OperandSpec::Nothing],
                    [OperandSpec::Rd, OperandSpec::Nothing, OperandSpec::Nothing],
                    [OperandSpec::Rd, OperandSpec::Nothing, OperandSpec::Nothing],
                    [OperandSpec::Rd, OperandSpec::Nothing, OperandSpec::Nothing],
                    [OperandSpec::Rs, OperandSpec::Nothing, OperandSpec::Nothing],
                    [OperandSpec::Rs, OperandSpec::Rt, OperandSpec::Rd],
                    [OperandSpec::Nothing, OperandSpec::Nothing, OperandSpec::Nothing],
                    [OperandSpec::Rs, OperandSpec::Rt, OperandSpec::Rd],
                    [OperandSpec::Rs, OperandSpec::Rt, OperandSpec::Rd],
                ][opc as usize];
            }

            instruction.opcode = unsafe {
                mem::transmute::<u8, Opcode>(0b01_000000 | opc)
            };
        } else if opc == REG_IMM {
            let opc = ((word >> 16) & 0b11111) as u8 | 0b10_000000;
            // reject all 0b11xxx patterns, and the right half of table A-40, section REGIMM,
            // except two outliers.
            if opc >= 0b10_011000 || (opc & 0b00_00100 > 0 && (opc != 0b10_001100 || opc != 0b10_001110)) {
                return Err(DecodeError::InvalidOpcode);
            }
            instruction.opcode = unsafe {
                mem::transmute::<u8, Opcode>(opc)
            };
            instruction.operands = [OperandSpec::Rs, OperandSpec::Rt, OperandSpec::Imm16];
        } else {
            if opc & 0b111100 == 0b011100 || opc == 0b111011 {
                // reserved opcode
                return Err(DecodeError::InvalidOpcode);
            }
            instruction.opcode = unsafe {
                mem::transmute::<u8, Opcode>(opc)
            };
            if opc < 0b000100 {
                // instruction is J or JAL, immediate offset
                instruction.operands = [OperandSpec::JOffset, OperandSpec::Nothing, OperandSpec::Nothing];
            } else if opc < 0b001000 {
                if opc < 0b000110 {
                    instruction.operands = [OperandSpec::Rs, OperandSpec::Rt, OperandSpec::Imm16];
                } else {
                    instruction.operands = [OperandSpec::Rs, OperandSpec::Imm16, OperandSpec::Nothing];
                }
            } else if opc < 0b010000 {
                if opc == 0b001111 {
                    instruction.operands = [OperandSpec::Rt, OperandSpec::Imm16, OperandSpec::Nothing];
                } else {
                    instruction.operands = [OperandSpec::Rt, OperandSpec::Rs, OperandSpec::Imm16];
                }
            } else if opc < 0b010100 {
                instruction.operands = [OperandSpec::CoprocessorOpcode, OperandSpec::Nothing, OperandSpec::Nothing];
            } else if opc < 0b011000 {
                if opc < 0b010110 {
                    instruction.operands = [OperandSpec::Rs, OperandSpec::Rt, OperandSpec::Imm16];
                } else {
                    instruction.operands = [OperandSpec::Rs, OperandSpec::Imm16, OperandSpec::Nothing];
                }
            } else if opc < 0b100000 {
                if opc < 0b011010 {
                    instruction.operands = [OperandSpec::Rs, OperandSpec::Rt, OperandSpec::Imm16];
                } else {
                    instruction.operands = [OperandSpec::Rt, OperandSpec::BaseOffset, OperandSpec::Nothing];
                }
            } else {
                instruction.operands = [OperandSpec::Rt, OperandSpec::BaseOffset, OperandSpec::Nothing];
            }
        }
        return Ok(());
    }
}

// SLL
// SPECIAL  0      rt     rd    sa    SLL
// 000000   00000  XXXXX  XXXXX XXXXX 000000
//
// SRL
// SPECIAL  0      rt     rd     sa     SRL
// 000000   00000  XXXXX  XXXXX  XXXXX  000010
//
// SRA
// SPECIAL  0      rt     rd     sa     SRA
// 000000   00000  XXXXX  XXXXX  XXXXX  000011
//
// SLLV
// SPECIAL  rs     rt     rd    0     SLLV
// 000000   XXXXX  XXXXX  XXXXX 00000 000100
//
// SRLV
// SPECIAL  rs     rt     rd     0      SRLV
// 000000   XXXXX  XXXXX  XXXXX  00000  000110
//
// SRAV
// SPECIAL  rs     rt     rd     0      SRAV
// 000000   XXXXX  XXXXX  XXXXX  00000  000111
//
// JR              v-----v---  reserved? might not be? unknown
// SPECIAL  rs                      JR
// 000000   XXXXX  000000000000000  001000
//
// JALR            v-------------v---  reserved? might not be? unknown
// SPECIAL  rs            rd            JALR
// 000000   XXXXX  00000  XXXXX  00000  001001
//
// MOVZ
// SPECIAL  rs     rt     rd     0      MOVZ
// 000000   XXXXX  XXXXX  XXXXX  00000  001010
//
// MOVN
// SPECIAL  rs     rt     rd     0      MOVN
// 000000   XXXXX  XXXXX  XXXXX  00000  001011
//
// SYSCALL
// SPECIAL  Code                  SYSCALL
// 000000   XXXXXXXXXXXXXXXXXXXX  001100
//
// BREAK
// SPECIAL  code                  BREAK
// 000000   XXXXXXXXXXXXXXXXXXXX  001101
//
// SYNC
// SPECIAL  0                stype  SYNC
// 000000   000000000000000  XXXXX  001111
//
// MFHI
// SPECIAL  0           rd     0      MFHI
// 000000   0000000000  XXXXX  00000  010000
//
// MTHI
// SPECIAL  rs     0                MTHI
// 000000   XXXXX  000000000000000  010001
//
// MFLO
// SPECIAL  0           rd     0      MFLO
// 000000   0000000000  XXXXX  00000  010010
//
// MTLO
// SPECIAL  rs     0                MTLO
// 000000   XXXXX  000000000000000  010011
//
// DSLLV
// SPECIAL  rs     rt     rd     0      DSLLV
// 000000   XXXXX  XXXXX  XXXXX  00000  010100
//
// DSRLV
// SPECIAL  rs     rt     rd     0      DSRLV
// 000000   XXXXX  XXXXX  XXXXX  00000  010110
//
// DSRAV
// SPECIAL  rs     rt     rd     0      DSRAV
// 000000   XXXXX  XXXXX  XXXXX  00000  010111
//
// MULT
// SPECIAL  rs     rt     0           MULT
// 000000   XXXXX  XXXXX  0000000000  011000
//
// MULTU
// SPECIAL  rs     rt     0           MULTU
// 000000   XXXXX  XXXXX  0000000000  011001
//
// DIV
// SPECIAL  rs     rt     0           DIV
// 000000   XXXXX  XXXXX  0000000000  011010
//
// DIVU
// SPECIAL  rs     rt     0           DIVU
// 000000   XXXXX  XXXXX  0000000000  011011
//
// DMULT
// SPECIAL  rs     rt     0           DMULT
// 000000   XXXXX  XXXXX  0000000000  011100
//
// DMULTU
// SPECIAL  rs     rt     0           DMULTU
// 000000   XXXXX  XXXXX  0000000000  011101
//
// DDIV
// SPECIAL  rs     rt     0           DDIV
// 000000   XXXXX  XXXXX  0000000000  011110
//
// DDIVU
// SPECIAL  rs     rt     0           DDIVU
// 000000   XXXXX  XXXXX  0000000000  011111
//
// ADD
// SPECIAL  rs     rt     rd     0      ADD
// 000000   XXXXX  XXXXX  XXXXX  00000  100000
//
// ADDU
// SPECIAL  rs     rt     rd     0      ADDU
// 000000   XXXXX  XXXXX  XXXXX  00000  100001
//
// SUB
// SPECIAL  rs     rt     rd     0      SUB
// 000000   XXXXX  XXXXX  XXXXX  00000  100010
//
// SUBU
// SPECIAL  rs     rt     rd     0      SUBU
// 000000   XXXXX  XXXXX  XXXXX  00000  100011
//
// AND
// SPECIAL  rs     rt     rd     0      ADDU
// 000000   XXXXX  XXXXX  XXXXX  00000  100100
//
// OR
// SPECIAL  rs     rt     rd     0      OR
// 000000   XXXXX  XXXXX  XXXXX  00000  100101
//
// XOR
// SPECIAL  rs     rt     rd     0      XOR
// 000000   XXXXX  XXXXX  XXXXX  00000  100110
//
// NOR
// SPECIAL  rs     rt     rd     0      NOR
// 000000   XXXXX  XXXXX  XXXXX  00000  100111
//
// SLT
// SPECIAL  rs     rt     rd     0      SLT
// 000000   XXXXX  XXXXX  XXXXX  00000  101010
//
// DADD
// SPECIAL  rs     rt     rd     0      DADD
// 000000   XXXXX  XXXXX  XXXXX  00000  101100
//
// DADDU
// SPECIAL  rs     rt     rd     0      DADDU
// 000000   XXXXX  XXXXX  XXXXX  00000  101101
//
// DSUB
// SPECIAL  rs     rt     rd     0      DSUB
// 000000   XXXXX  XXXXX  XXXXX  00000  101110
//
// DSUBU
// SPECIAL  rs     rt     rd     0      DSUBU
// 000000   XXXXX  XXXXX  XXXXX  00000  101111
//
// TGE
// SPECIAL  rs     rt     code        TGE
// 000000   XXXXX  XXXXX  0000000000  110000
//
// TGEU
// SPECIAL  rs     rt     code        TGEU
// 000000   XXXXX  XXXXX  0000000000  110001
//
// TLT
// SPECIAL  rs     rt     code        TLT
// 000000   XXXXX  XXXXX  0000000000  110010
//
// TLTU
// SPECIAL  rs     rt     code        TLTU
// 000000   XXXXX  XXXXX  0000000000  110011
//
// TEQ
// SPECIAL  rs     rt     code        TEQ
// 000000   XXXXX  XXXXX  0000000000  110100
//
// TNE
// SPECIAL  rs     rt     code        TNE
// 000000   XXXXX  XXXXX  0000000000  110110
//
// DSLL
// SPECIAL  0      rt     rd     sa     DSLL
// 000000   00000  XXXXX  XXXXX  XXXXX  111000
//
// DSRL
// SPECIAL  0      rt     rd     sa     DSRL
// 000000   00000  XXXXX  XXXXX  XXXXX  111010
//
// DSRA
// SPECIAL  0      rt     rd     sa     DSRA
// 000000   00000  XXXXX  XXXXX  XXXXX  111011
//
// DSLL32
// SPECIAL  0      rt     rd     sa     DSLL32
// 000000   00000  XXXXX  XXXXX  XXXXX  111100
//
// DSRL32
// SPECIAL  0      rt     rd     sa     DSRL32
// 000000   00000  XXXXX  XXXXX  XXXXX  111110
//
// DSRA32
// SPECIAL  0      rt     rd     sa     DSRA32
// 000000   00000  XXXXX  XXXXX  XXXXX  111111
//
// BLTZ
// REGIMM   rs     BLTZ   imm16
// 000001   XXXXX  00000  YYYYYYYYYYYYYYYY
//
// BGEZ
// REGIMM   rs     BGEZ   imm16
// 000001   XXXXX  00001  YYYYYYYYYYYYYYYY
//
// BLTZL
// REGIMM   rs     BLTZL  imm16
// 000001   XXXXX  00010  YYYYYYYYYYYYYYYY
//
// BGEZL
// REGIMM   rs     BGEZL  imm16
// 000001   XXXXX  00011  YYYYYYYYYYYYYYYY
//
// TGEI
// REGIMM   rs     TGEI   imm16
// 000001   XXXXX  01000  YYYYYYYYYYYYYYYY
//
// TGEIU
// REGIMM   rs     TGEIU  imm16
// 000001   XXXXX  01001  YYYYYYYYYYYYYYYY
//
// TLTI
// REGIMM   rs     TLTI   imm16
// 000001   XXXXX  01010  YYYYYYYYYYYYYYYY
//
// TLTIU
// REGIMM   rs     TLTIU  imm16
// 000001   XXXXX  01011  YYYYYYYYYYYYYYYY
//
// TEQI
// REGIMM   rs     TEQI   imm16
// 000001   XXXXX  01100  YYYYYYYYYYYYYYYY
//
// TNEI
// REGIMM   rs     TNEI   imm16
// 000001   XXXXX  01110  YYYYYYYYYYYYYYYY
//
// BLTZAL
// REGIMM   rs     BLTZAL imm16
// 000001   XXXXX  10000  YYYYYYYYYYYYYYYY
//
// BGEZAL
// REGIMM   rs     BGEZAL imm16
// 000001   XXXXX  10001  YYYYYYYYYYYYYYYY
//
// BLTZALL
// REGIMM   rs     BLTZALLimm16
// 000001   XXXXX  10010  YYYYYYYYYYYYYYYY
//
// BGEZALL
// REGIMM   rs     BGEZALLimm16
// 000001   XXXXX  10011  YYYYYYYYYYYYYYYY
//
// J        instr_index
// 000010   XXXXXXXXXXXXXXXXXXXXXXXXXX
//
// JAL      instr_index
// 000011   XXXXXXXXXXXXXXXXXXXXXXXXXX
//
// BEQ      rs     rt     imm16
// 000100   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// BNE      rs     rt     imm16
// 000101   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// BLEZ     rs     0      imm16
// 000110   XXXXX  00000  YYYYYYYYYYYYYYYY
//
// BGTZ     rs     0      imm16
// 000111   XXXXX  00000  YYYYYYYYYYYYYYYY
//
// ADDI     rs     rt     imm16
// 001000   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// ADDIU    rs     rt     imm16
// 001001   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// SLTI     rs     rt     imm16
// 001010   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// SLTII    rs     rt     imm16
// 001011   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// ANDI     rs     rt     imm16
// 001100   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// ORI      rs     rt     imm16
// 001101   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// XORI     rs     rt     imm16
// 001110   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// LUI      0      rt     imm16
// 001111   00000  XXXXX  YYYYYYYYYYYYYYYY
//
// COPz     cop_fun
// 0100zz   XXXXXXXXXXXXXXXXXXXXXXXXXX
//
// BEQL     rs     rt     imm16
// 010100   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// BNEL     rs     rt     imm16
// 010101   XXXXX  XXXXX  YYYYYYYYYYYYYYYY
//
// BLEZL    rs     0      imm16
// 010110   XXXXX  00000  YYYYYYYYYYYYYYYY
//
// BGTZL    rs     0      imm16
// 010111   XXXXX  00000  YYYYYYYYYYYYYYYY
//
// DADDI    rs     rt     imm16
// 011000   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// DADDIU   rs     rt     imm16
// 011001   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LDL      base   rt     offset
// 011010   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LDR      base   rt     offset
// 011011   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LB       base   rt     offset
// 100000   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LH       base   rt     offset
// 100001   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LWL      base   rt     offset
// 100010   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LW       base   rt     offset
// 100011   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LHU      base   rt     offset
// 100101   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LWR      base   rt     offset
// 100110   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LWU      base   rt     offset
// 100111   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LBU      base   rt     offset
// 100100   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// SB       base   rt     offset
// 101000   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LD       base   rt     offset
// 110111   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LDCz     base   rt     offset
// 1101zz   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LL       base   rt     offset
// 110000   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LWCz     base   rt     offset
// 1100zz   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//     ^-{01,10}
// PREF     base   hint   offset
// 110011   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// LLD      base   rt     offset
// 110100   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// SC       base   rt     offset
// 111000   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// SCD      base   rt     offset
// 111100   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// SD       base   rt     offset
// 111111   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// SDCz     base   rt     offset
// 1111zz   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// SDL      base   rt     offset
// 101100   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// SDR      base   rt     offset
// 101101   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// SH       base   rt     offset
// 101001   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// SW       base   rt     offset
// 101011   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// SWCz     base   rt     offset
// 1110zz   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// SWL      base   rt     offset
// 101010   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
//
// SWR      base   rt     offset
// 101110   XXXXX  XXXXX  XXXXXXXXXXXXXXXX
