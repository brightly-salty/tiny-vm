#![warn(clippy::all, clippy::pedantic, clippy::nursery)]
mod asm;
mod cpu;
use asm::assemble;
use cpu::Cpu;

pub fn run_assembly(s: &str) {
    let machine_code = assemble(s);
    let mut cpu = Cpu::new();
    cpu.parse_machine_code(&machine_code);
    cpu.run();
}
