use crate::types::{Address, Byte};
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct AsmLabel(String);

#[derive(Clone, Debug)]
enum AsmOperand {
    Direct(AsmLabel),   // must match operand
    Immediate(Address), // 0-999
}

#[derive(Debug, Clone)]
struct AsmOpcode {
    ln: usize,
    oc: AsmOc,
}

#[derive(Debug, Clone)]
enum AsmOc {
    Stop,
    Ld(AsmOperand),
    Ldi(AsmOperand),
    Lda(AsmOperand),
    St(AsmOperand),
    Sti(AsmOperand),
    Add(AsmOperand),
    Sub(AsmOperand),
    Mul(AsmOperand),
    Div(AsmOperand),
    In,
    Out,
    Jmp(AsmOperand),
    Jg(AsmOperand),
    Jl(AsmOperand),
    Je(AsmOperand),
    Call(AsmOperand),
    Ret,
    Push_,
    Push(AsmOperand),
    Pop_,
    Pop(AsmOperand),
    Ldparam(AsmOperand),
    Jge(AsmOperand),
    Jle(AsmOperand),
    Jne(AsmOperand),
    Pusha(AsmOperand),
    Dc(String),
    Db(Byte),
    Ds(u32),
    CallPrintInteger,
    CallPrintString,
    CallInputInteger,
    CallInputString,
}

struct AsmInstruction {
    label: Option<AsmLabel>,
    opcode: Option<AsmOpcode>,
}

impl AsmOpcode {
    pub fn len(&self) -> u16 {
        match &self.oc {
            AsmOc::Dc(s) => u16::try_from(s.chars().count()).unwrap().saturating_add(1),
            AsmOc::Ds(n) => u16::try_from(*n).unwrap(),
            _ => 1,
        }
    }
}

type SymbolTable = HashMap<Address, String>;

type SourceMap = HashMap<Address, usize>;

#[derive(Debug)]
struct AsmLabelMap {
    inner: HashMap<AsmLabel, Address>,
}

impl AsmLabelMap {
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn get(&self, ln: usize, label: &AsmLabel) -> Result<Address, String> {
        self.inner
            .get(label)
            .copied()
            .ok_or_else(|| format!("unknown operand at line {ln}: '{}'", label.0))
    }

    pub fn insert(&mut self, label: AsmLabel, address: Address) {
        self.inner.insert(label, address);
    }
}

fn create_symbol_table(map: AsmLabelMap) -> SymbolTable {
    let mut symbols = HashMap::new();

    symbols.insert(Address(900), "printInteger".to_owned());
    symbols.insert(Address(925), "printString".to_owned());
    symbols.insert(Address(950), "inputInteger".to_owned());
    symbols.insert(Address(975), "inputString".to_owned());
    for (k, v) in map.inner {
        symbols.insert(v, k.0);
    }
    symbols
}

/// # Errors
///
/// Will return `Err` if there was an error
pub fn assemble(asm: &str) -> Result<(SymbolTable, SourceMap, Vec<Byte>), String> {
    let lines = asm.lines();
    let parsed_instructions: Result<Vec<_>, _> = lines
        .enumerate()
        .map(|(n, s)| line_to_instruction(n, s))
        .collect();
    let (instructions, map, source_map) = first_pass(&parsed_instructions?);
    let machine_code = create_machine_code(&map, instructions)?;
    let symbol_table = create_symbol_table(map);
    Ok((symbol_table, source_map, machine_code))
}

fn line_to_instruction(ln: usize, mut line: &str) -> Result<AsmInstruction, String> {
    use AsmOperand::{Direct, Immediate};
    line = line.split(';').next().unwrap();
    let split_by_colon: Vec<_> = line.split(':').collect();
    let mut label = None;
    let mut opcode = None;
    let rest = if split_by_colon.len() >= 2 {
        label = Some(AsmLabel(split_by_colon[0].trim().to_ascii_lowercase()));
        split_by_colon[1..].join(":")
    } else {
        split_by_colon[0].to_string()
    };
    let opcode_parts: Vec<&str> = rest.split_whitespace().collect();
    if opcode_parts.len() == 1 {
        opcode = Some(AsmOpcode {
            ln,
            oc: match opcode_parts[0].to_ascii_lowercase().as_str() {
                "stop" => AsmOc::Stop,
                "in" => AsmOc::In,
                "out" => AsmOc::Out,
                "ret" => AsmOc::Ret,
                "push" => AsmOc::Push_,
                "pop" => AsmOc::Pop_,
                s => return Err(format!("unrecognized opcode at line {ln}: {s}")),
            },
        });
    } else if opcode_parts.len() >= 2 {
        let operand_str = opcode_parts[1..].join(" ");
        let operand = operand_str.parse::<u16>().map(Address).map_or_else(
            |_| Direct(AsmLabel(operand_str.to_ascii_lowercase())),
            Immediate,
        );
        opcode = Some(AsmOpcode {
            ln,
            oc: match opcode_parts[0].to_ascii_lowercase().as_str() {
                "ld" => AsmOc::Ld(operand),
                "ldi" => AsmOc::Ldi(operand),
                "lda" => AsmOc::Lda(operand),
                "st" => AsmOc::St(operand),
                "sti" => AsmOc::Sti(operand),
                "add" => AsmOc::Add(operand),
                "sub" => AsmOc::Sub(operand),
                "mul" => AsmOc::Mul(operand),
                "div" => AsmOc::Div(operand),
                "jmp" => AsmOc::Jmp(operand),
                "jg" => AsmOc::Jg(operand),
                "jl" => AsmOc::Jl(operand),
                "je" => AsmOc::Je(operand),
                "call" => match operand_str.to_ascii_lowercase().as_str() {
                    "printinteger" => AsmOc::CallPrintInteger,
                    "printstring" => AsmOc::CallPrintString,
                    "inputinteger" => AsmOc::CallInputInteger,
                    "inputstring" => AsmOc::CallInputString,
                    _ => AsmOc::Call(operand),
                },
                "ldparam" => AsmOc::Ldparam(operand),
                "jge" => AsmOc::Jge(operand),
                "jle" => AsmOc::Jle(operand),
                "jne" => AsmOc::Jne(operand),
                "push" => AsmOc::Push(operand),
                "pop" => AsmOc::Pop(operand),
                "pusha" => AsmOc::Pusha(operand),
                "dc" => AsmOc::Dc(
                    operand_str[1..operand_str.len().saturating_sub(1)].replace("\\n", "\n"),
                ),
                "db" => AsmOc::Db(Byte::new(operand_str.parse().unwrap())),
                "ds" => AsmOc::Ds(operand_str.parse().unwrap()),
                s => return Err(format!("unrecognized operand at line {ln}: {s}")),
            },
        });
    }

    Ok(AsmInstruction { label, opcode })
}

fn first_pass(input: &[AsmInstruction]) -> (Vec<AsmOpcode>, AsmLabelMap, SourceMap) {
    let mut instructions = vec![];
    let mut i = 0;
    let mut stored_label = None;
    let mut label_map = AsmLabelMap::new();
    let mut source_map = HashMap::new();
    let mut address = Address(0);
    while i < input.len() {
        let cur = &input[i];
        match (&cur.label, &cur.opcode) {
            (None, None) => {}
            (Some(label), None) => {
                stored_label = Some(label.clone());
            }
            (Some(label), Some(oc)) => {
                label_map.insert(label.clone(), address);
                source_map.insert(address, oc.ln);
                instructions.push(oc.clone());
                address = Address(address.0.saturating_add(oc.len()));
            }
            (None, Some(oc)) => {
                let label = stored_label.take();
                instructions.push(oc.clone());
                source_map.insert(address, oc.ln);
                if let Some(l) = label {
                    label_map.insert(l, address);
                }
                address = Address(address.0.saturating_add(oc.len()));
            }
        }
        i = i.saturating_add(1);
    }
    (instructions, label_map, source_map)
}

fn create_machine_code(map: &AsmLabelMap, opcodes: Vec<AsmOpcode>) -> Result<Vec<Byte>, String> {
    let mut v = Vec::new();
    for opcode in opcodes {
        opcode_to_machine(&mut v, map, opcode)?;
    }
    Ok(v)
}

fn add_instruction(v: &mut Vec<Byte>, x: i32, o: Address) {
    v.push(Byte::new(x * 1000 + o.0 as i32));
}

fn opcode_to_machine(
    v: &mut Vec<Byte>,
    map: &AsmLabelMap,
    opcode: AsmOpcode,
) -> Result<(), String> {
    use AsmOperand::{Direct, Immediate};
    match opcode.oc {
        AsmOc::Stop => v.push(Byte(0)),
        AsmOc::Ld(Direct(label)) => add_instruction(v, 1, map.get(opcode.ln, &label)?),
        AsmOc::Ld(Immediate(n)) => add_instruction(v, 91, n),
        AsmOc::Ldi(Direct(label)) => add_instruction(v, 2, map.get(opcode.ln, &label)?),
        AsmOc::Lda(Direct(label)) => add_instruction(v, 3, map.get(opcode.ln, &label)?),
        AsmOc::St(Direct(label)) => add_instruction(v, 4, map.get(opcode.ln, &label)?),
        AsmOc::Sti(Direct(label)) => add_instruction(v, 5, map.get(opcode.ln, &label)?),
        AsmOc::Add(Direct(label)) => add_instruction(v, 6, map.get(opcode.ln, &label)?),
        AsmOc::Add(Immediate(n)) => add_instruction(v, 96, n),
        AsmOc::Sub(Direct(label)) => add_instruction(v, 7, map.get(opcode.ln, &label)?),
        AsmOc::Sub(Immediate(n)) => add_instruction(v, 97, n),
        AsmOc::Mul(Direct(label)) => add_instruction(v, 8, map.get(opcode.ln, &label)?),
        AsmOc::Mul(Immediate(n)) => add_instruction(v, 98, n),
        AsmOc::Div(Direct(label)) => add_instruction(v, 9, map.get(opcode.ln, &label)?),
        AsmOc::Div(Immediate(n)) => add_instruction(v, 99, n),
        AsmOc::In => add_instruction(v, 10, Address(0)),
        AsmOc::Out => add_instruction(v, 11, Address(0)),
        AsmOc::Jmp(Direct(label)) => add_instruction(v, 12, map.get(opcode.ln, &label)?),
        AsmOc::Jg(Direct(label)) => add_instruction(v, 13, map.get(opcode.ln, &label)?),
        AsmOc::Jl(Direct(label)) => add_instruction(v, 14, map.get(opcode.ln, &label)?),
        AsmOc::Je(Direct(label)) => add_instruction(v, 15, map.get(opcode.ln, &label)?),
        AsmOc::Call(Direct(label)) => add_instruction(v, 16, map.get(opcode.ln, &label)?),
        AsmOc::Ret => v.push(Byte::new(17000)),
        AsmOc::Push_ => v.push(Byte::new(18000)),
        AsmOc::Pop_ => v.push(Byte::new(19000)),
        AsmOc::Ldparam(Immediate(n)) => add_instruction(v, 20, n),
        AsmOc::Jge(Direct(label)) => add_instruction(v, 21, map.get(opcode.ln, &label)?),
        AsmOc::Jle(Direct(label)) => add_instruction(v, 22, map.get(opcode.ln, &label)?),
        AsmOc::Jne(Direct(label)) => add_instruction(v, 23, map.get(opcode.ln, &label)?),
        AsmOc::Push(Direct(label)) => add_instruction(v, 24, map.get(opcode.ln, &label)?),
        AsmOc::Pop(Direct(label)) => add_instruction(v, 25, map.get(opcode.ln, &label)?),
        AsmOc::Pusha(Direct(label)) => add_instruction(v, 26, map.get(opcode.ln, &label)?),
        AsmOc::CallPrintInteger => v.push(Byte::new(16900)),
        AsmOc::CallPrintString => v.push(Byte::new(16925)),
        AsmOc::CallInputInteger => v.push(Byte::new(16950)),
        AsmOc::CallInputString => v.push(Byte::new(16975)),
        AsmOc::Db(b) => v.push(b),
        AsmOc::Ds(n) => {
            for _ in 0..n {
                v.push(Byte::new(0));
            }
        }
        AsmOc::Dc(string) => {
            let cs: Vec<_> = string.chars().collect();
            v.push(Byte::new(cs[0] as i32));
            for c in &cs[1..] {
                v.push(Byte::new(*c as i32));
            }
            v.push(Byte::new(0));
        }
        x => {
            return Err(format!(
                "Do not know how to execute {x:?} at line {}",
                opcode.ln
            ))
        }
    };
    Ok(())
}
