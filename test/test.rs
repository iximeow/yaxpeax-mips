#[test]
fn test_arithmetic() {
// 21080008 addi t0, t0, 8
// 2129fff8 addi t1, t1, -8
// 2404010c addiu a0, zero, 0x10c
// 2508ae20 addui t0, t0, -0x51e0
// 352923a0 ori t1, t1, 0x23a0
// 00042300 sll a0, a0, 0xc
}
#[test]
fn test_br() {
// 1520fffc bnez t1, 0x80001010
// 01400008 jr t2
// 03e00008 jr ra
// 10000008 b 0x8000171c
// 0c0005ab jal 0x800016ac
}
#[test]
fn test_cmp() {
// slt
// sltu
// slti
// 2de10007 sltiu at, t7, 1
}
#[test]
fn test_mov() {
// 8fbf0000 lw ra, (sp)
// 3c088009 lui t0, 0x8009
// 3c090002 lui t1, 2
// 6fc07f00 ldr zero, 0x7f00(fp)
// a429d266 sh t1, -0x2d9a(at)
// ad000000 sw zero, (t0)
// ad000004 sw zero, 4(t0)
// 02002025 move a0, s0
}
#[test]
fn test_misc() {
// 00000000 nop
// 40882800 mtc0 t0, a1, 0
// 42000002 tlbwi
// 42000008 tlbp
// 40085000 mfc t0, t2, 0
}
