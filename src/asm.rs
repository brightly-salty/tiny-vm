use std::collections::HashMap;
use std::fmt::Write;
use crate::types::Address;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct AsmLabel(String);

#[derive(Clone, Debug)]
enum AsmOperand {
    Direct(AsmLabel),  // must match operand
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
    Db(i32),
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

type SymbolTable = HashMap<String, String>;

type SourceMap = HashMap<Address, usize>;

#[derive(Debug)]
struct AsmLabelMap {
    inner: HashMap<AsmLabel, String>,
}

impl AsmLabelMap {
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn get(&self, ln: usize, label: &AsmLabel) -> Result<&String, String> {
        self.inner
            .get(label)
            .ok_or_else(|| format!("unknown operand at line {ln}: '{}'", label.0))
    }

    pub fn insert(&mut self, label: AsmLabel, address: Address) {
        self.inner.insert(label, pad_num(address));
    }
}

fn create_symbol_table(map: AsmLabelMap) -> SymbolTable {
    let mut symbols = HashMap::new();

    symbols.insert("900".to_owned(), "printInteger".to_owned());
    symbols.insert("925".to_owned(), "printString".to_owned());
    symbols.insert("950".to_owned(), "inputInteger".to_owned());
    symbols.insert("975".to_owned(), "inputString".to_owned());
    for (k, v) in map.inner {
        symbols.insert(v, k.0);
    }
    symbols
}

/// # Errors
///
/// Will return `Err` if there was an error
pub fn assemble(asm: &str) -> Result<(SymbolTable, SourceMap, String), String> {
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
                "dc" => {
                    AsmOc::Dc(operand_str[1..operand_str.len().saturating_sub(1)].replace("\\n", "\n"))
                }
                "db" => AsmOc::Db(operand_str.parse().unwrap()),
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

fn pad_num(n: Address) -> String {
    let mut s = n.0.to_string();
    debug_assert!(s.len() <= 3);
    while s.len() < 3 {
        s = format!("0{s}");
    }
    s
}

fn pad_five(n: i32) -> String {
    let mut s = n.to_string();
    debug_assert!(s.len() <= 5);
    while s.len() < 5 {
        s = format!("0{s}");
    }
    s
}

fn create_machine_code(map: &AsmLabelMap, opcodes: Vec<AsmOpcode>) -> Result<String, String> {
    let mut s = String::new();
    let mut address = Address(0);
    for opcode in opcodes {
        address = opcode_to_machine(&mut s, map, address, opcode)?;
    }
    Ok(s)
}

fn opcode_to_machine(
    s: &mut String,
    map: &AsmLabelMap,
    a: Address,
    opcode: AsmOpcode,
) -> Result<Address, String> {
    use AsmOperand::{Direct, Immediate};
    write!(s, "{} ", pad_num(a)).unwrap();
    let mut new_address = Address(a.0.saturating_add(1));
    match opcode.oc {
        AsmOc::Stop => writeln!(s, "00000").unwrap(),
        AsmOc::Ld(Direct(label)) => writeln!(s, "01{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::Ld(Immediate(n)) => writeln!(s, "91{}", pad_num(n)).unwrap(),
        AsmOc::Ldi(Direct(label)) => writeln!(s, "02{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::Lda(Direct(label)) => writeln!(s, "03{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::St(Direct(label)) => writeln!(s, "04{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::Sti(Direct(label)) => writeln!(s, "05{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::Add(Direct(label)) => writeln!(s, "06{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::Add(Immediate(n)) => writeln!(s, "96{}", pad_num(n)).unwrap(),
        AsmOc::Sub(Direct(label)) => writeln!(s, "07{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::Sub(Immediate(n)) => writeln!(s, "97{}", pad_num(n)).unwrap(),
        AsmOc::Mul(Direct(label)) => writeln!(s, "08{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::Mul(Immediate(n)) => writeln!(s, "98{}", pad_num(n)).unwrap(),
        AsmOc::Div(Direct(label)) => writeln!(s, "09{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::Div(Immediate(n)) => writeln!(s, "99{}", pad_num(n)).unwrap(),
        AsmOc::In => writeln!(s, "10000").unwrap(),
        AsmOc::Out => writeln!(s, "11000").unwrap(),
        AsmOc::Jmp(Direct(label)) => writeln!(s, "12{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::Jg(Direct(label)) => writeln!(s, "13{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::Jl(Direct(label)) => writeln!(s, "14{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::Je(Direct(label)) => writeln!(s, "15{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::Call(Direct(label)) => writeln!(s, "16{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::Ret => writeln!(s, "17000").unwrap(),
        AsmOc::Push_ => writeln!(s, "18000").unwrap(),
        AsmOc::Pop_ => writeln!(s, "19000").unwrap(),
        AsmOc::Ldparam(Immediate(n)) => writeln!(s, "20{}", pad_num(n)).unwrap(),
        AsmOc::Jge(Direct(label)) => writeln!(s, "21{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::Jle(Direct(label)) => writeln!(s, "22{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::Jne(Direct(label)) => writeln!(s, "23{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::Push(Direct(label)) => writeln!(s, "24{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::Pop(Direct(label)) => writeln!(s, "25{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::Pusha(Direct(label)) => writeln!(s, "26{}", map.get(opcode.ln, &label)?).unwrap(),
        AsmOc::CallPrintInteger => writeln!(s, "16900").unwrap(),
        AsmOc::CallPrintString => writeln!(s, "16925").unwrap(),
        AsmOc::CallInputInteger => writeln!(s, "16950").unwrap(),
        AsmOc::CallInputString => writeln!(s, "16975").unwrap(),
        AsmOc::Db(n) => writeln!(s, "{}", pad_five(n)).unwrap(),
        AsmOc::Ds(n) => {
            writeln!(s, "00000").unwrap();
            let mut x = Address(a.0.saturating_add(1));
            for _ in 1..n {
                writeln!(s, "{} 00000", pad_num(x)).unwrap();
                x = Address(x.0.saturating_add(1));
            }
            new_address = x;
        }
        AsmOc::Dc(string) => {
            let cs: Vec<_> = string.chars().collect();
            writeln!(s, "{}", pad_five(cs[0] as i32)).unwrap();
            let mut x = a;
            for c in &cs[1..] {
                writeln!(s, "{} {}", pad_num(x), pad_five(*c as i32)).unwrap();
                x = Address(x.0.saturating_add(1));
            }
            writeln!(s, "{} 00000", pad_num(x)).unwrap();
            new_address = Address(x.0.saturating_add(1));
        }
        x => {
            return Err(format!(
                "Do not know how to execute {x:?} at line {}",
                opcode.ln
            ))
        }
    };
    Ok(new_address)
}
