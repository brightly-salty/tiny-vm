use crate::types::{Address, Byte, TinyError, TinyResult};
use rand::thread_rng;
use std::collections::{BTreeMap, HashMap};

type Operand = String;

#[derive(Debug, Clone)]
struct Line {
    ln: usize,
    oc: Instruction,
}

#[derive(Debug, Clone)]
enum Instruction {
    Easy(Vec<Byte>),
    Hard(Opcode, Operand),
}

#[derive(Debug, Clone)]
enum Opcode {
    Ld,
    Ldi,
    Lda,
    St,
    Sti,
    Add,
    Sub,
    Mul,
    Div,
    Jmp,
    Jg,
    Jl,
    Je,
    Call,
    Push,
    Pop,
    Jge,
    Jle,
    Jne,
    Pusha,
}

type SymbolTable = BTreeMap<Address, String>;

type SourceMap = HashMap<Address, usize>;

type LabelMap = HashMap<String, Address>;

/// # Errors
///
/// Will return `Err` if assembly was invalid
pub fn assemble(asm: &str) -> TinyResult<(SymbolTable, SourceMap, Vec<Byte>)> {
    let mut opcodes = Vec::with_capacity(asm.lines().count());
    let mut stored_label = None;
    let mut map = HashMap::with_capacity(asm.lines().count());
    let mut source_map = HashMap::with_capacity(asm.lines().count());
    let mut symbols = BTreeMap::new();
    symbols.insert(Address(900), "printInteger".to_owned());
    symbols.insert(Address(925), "printString".to_owned());
    symbols.insert(Address(950), "inputInteger".to_owned());
    symbols.insert(Address(975), "inputString".to_owned());
    let mut address = Address(0);
    for (ln, line) in asm.lines().enumerate() {
        let (label, opcode_parts) = parse(line);
        if let Some(l) = label.or_else(|| stored_label.take()) {
            if opcode_parts.is_empty() {
                stored_label = Some(l);
            } else {
                map.insert(l.clone(), address);
                symbols.insert(address, l);
            }
        }
        if !opcode_parts.is_empty() {
            let (len, opcode) = parse_opcode(ln, &opcode_parts)?;

            opcodes.push(opcode.clone());
            source_map.insert(address, ln);
            address = Address(address.0.saturating_add(len));
        }
    }
    let mut v = Vec::with_capacity(opcodes.len());
    for opcode in opcodes {
        match opcode.oc {
            Instruction::Easy(bytes) => v.extend(bytes),
            Instruction::Hard(oc, label) => match oc {
                Opcode::Ld => v.push(instruction(1, get_map(&map, opcode.ln, &label)?)),
                Opcode::Ldi => v.push(instruction(2, get_map(&map, opcode.ln, &label)?)),
                Opcode::Lda => v.push(instruction(3, get_map(&map, opcode.ln, &label)?)),
                Opcode::St => v.push(instruction(4, get_map(&map, opcode.ln, &label)?)),
                Opcode::Sti => v.push(instruction(5, get_map(&map, opcode.ln, &label)?)),
                Opcode::Add => v.push(instruction(6, get_map(&map, opcode.ln, &label)?)),
                Opcode::Sub => v.push(instruction(7, get_map(&map, opcode.ln, &label)?)),
                Opcode::Mul => v.push(instruction(8, get_map(&map, opcode.ln, &label)?)),
                Opcode::Div => v.push(instruction(9, get_map(&map, opcode.ln, &label)?)),
                Opcode::Jmp => v.push(instruction(12, get_map(&map, opcode.ln, &label)?)),
                Opcode::Jg => v.push(instruction(13, get_map(&map, opcode.ln, &label)?)),
                Opcode::Jl => v.push(instruction(14, get_map(&map, opcode.ln, &label)?)),
                Opcode::Je => v.push(instruction(15, get_map(&map, opcode.ln, &label)?)),
                Opcode::Call => {
                    v.push(instruction(16, get_map(&map, opcode.ln, &label)?));
                }
                Opcode::Jge => v.push(instruction(21, get_map(&map, opcode.ln, &label)?)),
                Opcode::Jle => v.push(instruction(22, get_map(&map, opcode.ln, &label)?)),
                Opcode::Jne => v.push(instruction(23, get_map(&map, opcode.ln, &label)?)),
                Opcode::Push => {
                    v.push(instruction(24, get_map(&map, opcode.ln, &label)?));
                }
                Opcode::Pop => v.push(instruction(25, get_map(&map, opcode.ln, &label)?)),
                Opcode::Pusha => {
                    v.push(instruction(26, get_map(&map, opcode.ln, &label)?));
                }
            },
        };
    }
    Ok((symbols, source_map, v))
}

fn instruction(x: i32, o: Address) -> Byte {
    Byte::new(x.saturating_mul(1000).saturating_add(i32::from(o.0)))
}

fn get_map(map: &LabelMap, ln: usize, label: &str) -> TinyResult<Address> {
    map.get(label)
        .copied()
        .ok_or_else(|| TinyError::UnknownOperand(ln, label.to_owned()))
}

fn parse(line: &str) -> (Option<String>, Vec<String>) {
    let line = if let Some((new_line, _)) = line.to_ascii_lowercase().split_once(';') {
        new_line.to_owned()
    } else {
        line.to_ascii_lowercase()
    };
    if let Some((label, rest)) = line.split_once(':') {
        (
            Some(label.trim().to_owned()),
            rest.split_whitespace().map(ToOwned::to_owned).collect(),
        )
    } else {
        (
            None,
            line.split_whitespace().map(ToOwned::to_owned).collect(),
        )
    }
}

fn parse_opcode(ln: usize, opcode_parts: &[String]) -> TinyResult<(u16, Line)> {
    let mut len = 1;
    let opcode = if opcode_parts.len() == 1 {
        Line {
            ln,
            oc: Instruction::Easy(vec![Byte(match opcode_parts[0].as_str() {
                "stop" => 0,
                "in" => 10_000,
                "out" => 11_000,
                "ret" => 17_000,
                "push" => 18_000,
                "pop" => 19_000,
                s => return Err(TinyError::UnknownOpcode(ln, s.to_owned())),
            })]),
        }
    } else {
        let operand_str = &opcode_parts[1..].join(" ");
        Line {
            ln,
            oc: if let Ok(imm) = operand_str.parse::<u16>().map(Address) {
                Instruction::Easy(match opcode_parts[0].as_str() {
                    "ld" => vec![instruction(91, imm)],
                    "add" => vec![instruction(96, imm)],
                    "sub" => vec![instruction(97, imm)],
                    "mul" => vec![instruction(98, imm)],
                    "div" => vec![instruction(99, imm)],
                    "ldparam" => vec![instruction(20, imm)],
                    "db" => vec![Byte(operand_str.parse().map_err(|_| {
                        TinyError::InvalidOperand(ln, "db".to_string(), operand_str.clone())
                    })?)],
                    "ds" => {
                        let n = operand_str.parse().map_err(|_| {
                            TinyError::InvalidOperand(ln, "ds".to_string(), operand_str.clone())
                        })?;
                        len = n;
                        let mut v = Vec::with_capacity(n as usize);
                        let mut rng = thread_rng();
                        for _ in 0..n {
                            v.push(Byte::random(&mut rng));
                        }
                        v
                    }
                    _ => {
                        return Err(TinyError::InvalidOperand(
                            ln,
                            opcode_parts[0].to_string(),
                            operand_str.clone(),
                        ))
                    }
                })
            } else {
                let operand = operand_str.to_owned();

                match opcode_parts[0].as_str() {
                    "ld" => Instruction::Hard(Opcode::Ld, operand),
                    "ldi" => Instruction::Hard(Opcode::Ldi, operand),
                    "lda" => Instruction::Hard(Opcode::Lda, operand),
                    "st" => Instruction::Hard(Opcode::St, operand),
                    "sti" => Instruction::Hard(Opcode::Sti, operand),
                    "add" => Instruction::Hard(Opcode::Add, operand),
                    "sub" => Instruction::Hard(Opcode::Sub, operand),
                    "mul" => Instruction::Hard(Opcode::Mul, operand),
                    "div" => Instruction::Hard(Opcode::Div, operand),
                    "jmp" => Instruction::Hard(Opcode::Jmp, operand),
                    "jg" => Instruction::Hard(Opcode::Jg, operand),
                    "jl" => Instruction::Hard(Opcode::Jl, operand),
                    "je" => Instruction::Hard(Opcode::Je, operand),
                    "call" => match operand_str.as_str() {
                        "printinteger" => Instruction::Easy(vec![Byte(16900)]),
                        "printstring" => Instruction::Easy(vec![Byte(16925)]),
                        "inputinteger" => Instruction::Easy(vec![Byte(16950)]),
                        "inputstring" => Instruction::Easy(vec![Byte(16975)]),
                        _ => Instruction::Hard(Opcode::Call, operand),
                    },
                    "jge" => Instruction::Hard(Opcode::Jge, operand),
                    "jle" => Instruction::Hard(Opcode::Jle, operand),
                    "jne" => Instruction::Hard(Opcode::Jne, operand),
                    "push" => Instruction::Hard(Opcode::Push, operand),
                    "pop" => Instruction::Hard(Opcode::Pop, operand),
                    "pusha" => Instruction::Hard(Opcode::Pusha, operand),
                    "dc" => {
                        let s = operand_str[1..operand_str.len().saturating_sub(1)]
                            .replace("\\n", "\n");

                        let mut v: Vec<_> = s.chars().map(|c| Byte(c as i32)).collect();
                        v.push(Byte::new(0));
                        len = u16::try_from(v.len()).map_err(|_| {
                            TinyError::InternalProgramError(
                                "dc string length larger than u16::MAX".to_owned(),
                            )
                        })?;
                        Instruction::Easy(v)
                    }
                    s => return Err(TinyError::UnknownOperand(ln, s.to_owned())),
                }
            },
        }
    };
    Ok((len, opcode))
}
