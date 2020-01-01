use std::fmt;

use crate::{Instruction, Opcode, Operand};

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.opcode == Opcode::OR {
            match (self.operand(&self.operands[0]), self.operand(&self.operands[1]), self.operand(&self.operands[2])) {
                (Some(a), Some(b), Some(Operand::Reg(0))) => {
                    return write!(f, "move {}, {}", a, b);
                }
                _ => {}
            }
        } else if self.opcode == Opcode::BEQ {
            match (self.operand(&self.operands[0]), self.operand(&self.operands[1]), self.operand(&self.operands[2])) {
                (Some(Operand::Reg(a)), Some(Operand::Reg(b)), Some(Operand::Imm(offs))) => {
                    if a == b {
                        if offs < 0 {
                            return write!(f, "b $-{:#x}", -offs);
                        } else {
                            return write!(f, "b $+{:#x}", offs);
                        }
                    }
                }
                _ => {}
            }
        } else if self.opcode == Opcode::SLL {
            match (self.operand(&self.operands[0]), self.operand(&self.operands[1]), self.operand(&self.operands[2])) {
                (Some(Operand::Reg(dest)), Some(Operand::Reg(src)), Some(Operand::Shift(0))) => {
                    // TODO: should this also test for dest == `zero`?
                    if dest == src {
                        return write!(f, "nop");
                    }
                }
                _ => { }
            }
        }

        fn display_operand(f: &mut fmt::Formatter, opcode: &Opcode, op: &Operand) -> fmt::Result {
            if *opcode == Opcode::LUI {
                // we show the immediate of LUI as an unsigned integer, becaue the docs say so.
                if let Operand::Imm(imm) = op {
                    return write!(f, "{:#x}", *imm as u16);
                }
            } else if let Operand::Imm(imm) = op {
                if *imm < 0 {
                    return write!(f, "-{:#x}", -imm);
                } else {
                    return write!(f, "{:#x}", imm);
                }
            }

            write!(f, "{}", op)
        }
        write!(f, "{}", self.opcode)?;

        let mut wrote_operand = false;
        for op in self.operands.iter() {
            match self.operand(op) {
                Some(op) => {
                    if wrote_operand {
                        write!(f, ", ")?;
                    } else {
                        write!(f, " ")?;
                        wrote_operand = true;
                    }
                    display_operand(f, &self.opcode, &op)?;
                }
                _ => {
                    return Ok(());
                }
            }
        }

        Ok(())
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Reg(reg) => {
                let name = [
                    "zero", "at",
                    "v0", "v1",
                    "a0", "a1", "a2", "a3",
                    "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7",
                    "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
                    "t8", "t9",
                    "k0", "k1",
                    "gp", "sp", "fp", "ra"
                ][*reg as usize];
                write!(f, "{}", name)
            }
            Operand::Imm(imm) => {
                write!(f, "{:#x}", imm)
            }
            Operand::BaseOffset(reg, offs) => {
                let name = [
                    "zero", "at",
                    "v0", "v1",
                    "a0", "a1", "a2", "a3",
                    "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7",
                    "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
                    "t8", "t9",
                    "k0", "k1",
                    "gp", "sp", "fp", "ra"
                ][*reg as usize];
                if *offs == 0 {
                    write!(f, "({})", name)
                } else {
                    if *offs < 0 {
                        write!(f, "-{:#x}({})", -offs, name)
                    } else {
                        write!(f, "{:#x}({})", offs, name)
                    }
                }
            }
            Operand::Shift(sa) => {
                write!(f, "{:#x}", sa)
            }
            Operand::LongImm(imm) => {
                write!(f, "{:#x}", imm)
            }
            Operand::JOffset(offs) => {
                write!(f, "$+{:#x}", offs)
            }
        }
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Opcode::J => write!(f, "j"),
            Opcode::JAL => write!(f, "jal"),
            Opcode::BEQ => write!(f, "beq"),
            Opcode::BNE => write!(f, "bne"),
            Opcode::BLEZ => write!(f, "blez"),
            Opcode::BGTZ => write!(f, "bgtz"),
            Opcode::ADDI => write!(f, "addi"),
            Opcode::ADDIU => write!(f, "addiu"),
            Opcode::SLTI => write!(f, "slti"),
            Opcode::SLTIU => write!(f, "sltiu"),
            Opcode::ANDI => write!(f, "andi"),
            Opcode::ORI => write!(f, "ori"),
            Opcode::XORI => write!(f, "xori"),
            Opcode::LUI => write!(f, "lui"),
            Opcode::COP1 => write!(f, "cop1"),
            Opcode::COP2 => write!(f, "cop2"),
            Opcode::COP3 => write!(f, "cop3"),
            Opcode::COP4 => write!(f, "cop4"),
            Opcode::BEQL => write!(f, "beql"),
            Opcode::BNEL => write!(f, "bnel"),
            Opcode::BLEZL => write!(f, "blezl"),
            Opcode::BGTZL => write!(f, "bgtzl"),
            Opcode::DADDI => write!(f, "daddi"),
            Opcode::DADDIU => write!(f, "daddiu"),
            Opcode::LDL => write!(f, "ldl"),
            Opcode::LDR => write!(f, "ldr"),
            Opcode::LB => write!(f, "lb"),
            Opcode::LH => write!(f, "lh"),
            Opcode::LWL => write!(f, "lwl"),
            Opcode::LW => write!(f, "lw"),
            Opcode::LBU => write!(f, "lbu"),
            Opcode::LHU => write!(f, "lhu"),
            Opcode::LWR => write!(f, "lwr"),
            Opcode::LWU => write!(f, "lwu"),
            Opcode::SB => write!(f, "sb"),
            Opcode::SH => write!(f, "sh"),
            Opcode::SWL => write!(f, "swl"),
            Opcode::SW => write!(f, "sw"),
            Opcode::SDL => write!(f, "sdl"),
            Opcode::SDR => write!(f, "sdr"),
            Opcode::SWR => write!(f, "swr"),
            Opcode::LL => write!(f, "ll"),
            Opcode::LWC1 => write!(f, "lwc1"),
            Opcode::LWC2 => write!(f, "lwc2"),
            Opcode::PREF => write!(f, "pref"),
            Opcode::LLD => write!(f, "lld"),
            Opcode::LDC1 => write!(f, "ldc1"),
            Opcode::LDC2 => write!(f, "ldc2"),
            Opcode::LD => write!(f, "ld"),
            Opcode::SC => write!(f, "sc"),
            Opcode::SWC1 => write!(f, "swc1"),
            Opcode::SWC2 => write!(f, "swc2"),
            Opcode::SCD => write!(f, "scd"),
            Opcode::SDC1 => write!(f, "sdc1"),
            Opcode::SDC2 => write!(f, "sdc2"),
            Opcode::SD => write!(f, "sd"),
            Opcode::SLL => write!(f, "sll"),
            Opcode::SRL => write!(f, "srl"),
            Opcode::SRA => write!(f, "sra"),
            Opcode::SLLV => write!(f, "sllv"),
            Opcode::SRLV => write!(f, "srlv"),
            Opcode::SRAV => write!(f, "srav"),
            Opcode::JR => write!(f, "jr"),
            Opcode::JALR => write!(f, "jalr"),
            Opcode::MOVZ => write!(f, "movz"),
            Opcode::MOVN => write!(f, "movn"),
            Opcode::SYSCALL => write!(f, "syscall"),
            Opcode::BREAK => write!(f, "break"),
            Opcode::SYNC => write!(f, "sync"),
            Opcode::MFHI => write!(f, "mfhi"),
            Opcode::MTHI => write!(f, "mthi"),
            Opcode::MFLO => write!(f, "mflo"),
            Opcode::MTLO => write!(f, "mtlo"),
            Opcode::DSLLV => write!(f, "dsllv"),
            Opcode::DSRLV => write!(f, "dsrlv"),
            Opcode::DSRAV => write!(f, "dsrav"),
            Opcode::MULT => write!(f, "mult"),
            Opcode::MULTU => write!(f, "multu"),
            Opcode::DIV => write!(f, "div"),
            Opcode::DIVU => write!(f, "divu"),
            Opcode::DMULT => write!(f, "dmult"),
            Opcode::DMULTU => write!(f, "dmultu"),
            Opcode::DDIV => write!(f, "ddiv"),
            Opcode::DDIVU => write!(f, "ddivu"),
            Opcode::ADD => write!(f, "add"),
            Opcode::ADDU => write!(f, "addu"),
            Opcode::SUB => write!(f, "sub"),
            Opcode::SUBU => write!(f, "subu"),
            Opcode::AND => write!(f, "and"),
            Opcode::OR => write!(f, "or"),
            Opcode::XOR => write!(f, "xor"),
            Opcode::NOR => write!(f, "nor"),
            Opcode::SLT => write!(f, "slt"),
            Opcode::DADD => write!(f, "dadd"),
            Opcode::DADDU => write!(f, "daddu"),
            Opcode::DSUB => write!(f, "dsub"),
            Opcode::DSUBU => write!(f, "dsubu"),
            Opcode::TGE => write!(f, "tge"),
            Opcode::TGEU => write!(f, "tgeu"),
            Opcode::TLT => write!(f, "tlt"),
            Opcode::TLTU => write!(f, "tltu"),
            Opcode::TEQ => write!(f, "teq"),
            Opcode::TNE => write!(f, "tne"),
            Opcode::DSLL => write!(f, "dsll"),
            Opcode::DSRL => write!(f, "dsrl"),
            Opcode::DSRA => write!(f, "dsra"),
            Opcode::DSLL32 => write!(f, "dsll32"),
            Opcode::DSRL32 => write!(f, "dsrl32"),
            Opcode::DSRA32 => write!(f, "dsra32"),
            Opcode::BLTZ => write!(f, "bltz"),
            Opcode::BGEZ => write!(f, "bgez"),
            Opcode::BLTZL => write!(f, "bltzl"),
            Opcode::BGEZL => write!(f, "bgezl"),
            Opcode::TGEI => write!(f, "tgei"),
            Opcode::TGEIU => write!(f, "tgeiu"),
            Opcode::TLTI => write!(f, "tlti"),
            Opcode::TLTIU => write!(f, "tltiu"),
            Opcode::TEQI => write!(f, "teqi"),
            Opcode::TNEI => write!(f, "tnei"),
            Opcode::BLTZAL => write!(f, "bltzal"),
            Opcode::BGEZAL => write!(f, "bgezal"),
            Opcode::BLTZALL => write!(f, "bltzall"),
            Opcode::BGEZALL => write!(f, "bgezall"),
        }
    }
}
