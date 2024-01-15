#![warn(
    clippy::cargo,
    clippy::complexity,
    clippy::correctness,
    clippy::perf,
    clippy::style,
    clippy::suspicious,
    clippy::pedantic,
    clippy::nursery,
    clippy::arithmetic_side_effects,
    clippy::format_push_string,
    clippy::if_then_some_else_none,
    clippy::missing_asserts_for_indexing,
    clippy::redundant_type_annotations,
    clippy::unwrap_in_result
)]
mod asm;
mod cpu;
pub use asm::assemble;
pub use cpu::Cpu;

/// # Errors
///
/// Will return an `Err` if the assembly could not be assembled
pub fn run_assembly(s: &str) -> Result<(), String> {
    let (map, machine_code) = assemble(s)?;
    let mut cpu = Cpu::new();
    cpu.parse_machine_code(map, &machine_code);
    cpu.run()
}
