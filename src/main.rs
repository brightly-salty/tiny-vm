use clap::Parser;
use std::fs::read_to_string;

/// Assembler and emulator for the Tiny CPU
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    filename: String,
}

fn main() {
    let args = Args::parse();
    let assembly = read_to_string(args.filename).expect("Could not open assembly file to read");
    tiny_vm::run_assembly(&assembly);
}
