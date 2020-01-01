extern crate yaxpeax_arch;
extern crate yaxpeax_mips;

use yaxpeax_arch::{Arch, Decoder, LengthedInstruction};
use yaxpeax_mips::{MIPS, Instruction, Opcode};

fn test_decode(data: [u8; 4], expected: Instruction) {
    let instr = <MIPS as Arch>::Decoder::default().decode(data.to_vec()).unwrap();
    assert!(
        instr == expected,
        "decode error for {:02x}{:02x}{:02x}{:02x}:\n  decoded: {:?}\n expected: {:?}\n",
        data[0], data[1], data[2], data[3],
        instr, expected
    );
}

fn test_display(data: [u8; 4], expected: &'static str) {
    let instr = <MIPS as Arch>::Decoder::default().decode(data.to_vec()).unwrap();
    let text = format!("{}", instr);
    assert!(
        text == expected,
        "display error for {:02x}{:02x}{:02x}{:02x}:\n  decoded: {:?}\n displayed: {}\n expected: {}\n",
        data[0], data[1], data[2], data[3],
        instr,
        text, expected
    );
}

#[test]
fn test_arithmetic() {
    test_display(
        [0x08, 0x00, 0x00, 0x21],
        "addi zero, t0, 0x8"
    );
    test_display(
        [0xf8, 0xff, 0x29, 0x21],
        "addi t1, t1, -0x8"
    );
    test_display(
        [0x0c, 0x01, 0x04, 0x24],
        "addiu a0, zero, 0x10c"
    );
    test_display(
        [0x20, 0xae, 0x08, 0x25],
        "addiu t0, t0, -0x51e0"
    );
    test_display(
        [0xa0, 0x23, 0x29, 0x35],
        "ori t1, t1, 0x23a0"
    );
    test_display(
        [0x00, 0x23, 0x04, 0x00],
        "sll a0, a0, 0xc"
    );
}
#[test]
fn test_br() {
    test_display(
        [0xfc, 0xff, 0x20, 0x15],
        "bne t1, zero, -0x4"
    );
    test_display(
        [0x08, 0x00, 0x40, 0x01],
        "jr t2"
    );
    test_display(
        [0x08, 0x00, 0xe0, 0x03],
        "jr ra"
    );
    test_display(
        [0x08, 0x00, 0x00, 0x10],
        "b $+0x8"
    );
    // TODO: this is inaccurate! this really is setting the low 28 bits of $pc, not branching
    // forward or something.
    test_display(
        [0xab, 0x05, 0x00, 0x0c],
        "jal $+0x16ac"
    );
}
#[test]
fn test_cmp() {
// slt
// sltu
// slti
    test_display(
        [0x07, 0x00, 0xe1, 0x2d],
        "sltiu at, t7, 0x7"
    );
}
#[test]
fn test_mov() {
    test_display(
        [0x00, 0x00, 0xbf, 0x8f],
        "lw ra, (sp)"
    );
    test_display(
        [0x09, 0x80, 0x08, 0x3c],
        "lui t0, 0x8009"
    );
    test_display(
        [0x02, 0x00, 0x09, 0x3c],
        "lui t1, 0x2"
    );
    test_display(
        [0x00, 0x7f, 0xc0, 0x6f],
        "ldr zero, 0x7f00(fp)"
    );
    test_display(
        [0x66, 0xd2, 0x29, 0xa4],
        "sh t1, -0x2d9a(at)"
    );
    test_display(
        [0x00, 0x00, 0x00, 0xad],
        "sw zero, (t0)"
    );
    test_display(
        [0x04, 0x00, 0x00, 0xad],
        "sw zero, 0x4(t0)"
    );
    test_display(
        [0x25, 0x20, 0x00, 0x02],
        "move a0, s0"
    );
}
#[test]
fn test_misc() {
    test_display(
        [0x00, 0x00, 0x00, 0x00],
        "nop"
    );
    /*test_display(
        [0x00, 0x28, 0x88, 0x40],
        "mtc0 t0, a1, 0"
    );*/
    test_display(
        [0x00, 0x28, 0x88, 0x40],
        "cop1 0x882800"
    );
    /*
    test_display(
        [0x02, 0x00, 0x00, 0x42],
        "tlbwi"
    );
    */
    test_display(
        [0x02, 0x00, 0x00, 0x42],
        "cop1 0x2000002"
    );
    /*
    test_display(
        [0x08, 0x00, 0x00, 0x42],
        "tlbp"
    );
    */
    test_display(
        [0x08, 0x00, 0x00, 0x42],
        "cop1 0x2000008"
    );
    /*
    test_display(
        [0x00, 0x50, 0x08, 0x40],
        "mfc t0, t2, 0"
    );
    */
    test_display(
        [0x00, 0x50, 0x08, 0x40],
        "cop1 0x85000"
    );
}
