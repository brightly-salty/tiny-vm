use std::collections::HashMap;
use std::fmt::Write;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct Label(String);

#[derive(Clone, Debug)]
enum Operand {
    Direct(Label),  // must match operand
    Immediate(u16), // 0-999
}

#[derive(Debug, Clone)]
struct Opcode {
    ln: usize,
    oc: Oc,
}

#[derive(Debug, Clone)]
enum Oc {
    Stop,
    Ld(Operand),
    Ldi(Operand),
    Lda(Operand),
    St(Operand),
    Sti(Operand),
    Add(Operand),
    Sub(Operand),
    Mul(Operand),
    Div(Operand),
    In,
    Out,
    Jmp(Operand),
    Jg(Operand),
    Jl(Operand),
    Je(Operand),
    Call(Operand),
    Ret,
    Push_,
    Push(Operand),
    Pop_,
    Pop(Operand),
    Ldparam(Operand),
    Jge(Operand),
    Jle(Operand),
    Jne(Operand),
    Pusha(Operand),
    Dc(String),
    Db(i32),
    Ds(u32),
    CallPrintInteger,
    CallPrintString,
    CallInputInteger,
    CallInputString,
}

struct Instruction {
    label: Option<Label>,
    opcode: Option<Opcode>,
}

impl Opcode {
    pub fn len(&self) -> u16 {
        match &self.oc {
            Oc::Dc(s) => u16::try_from(s.chars().count()).unwrap().saturating_add(1),
            Oc::Ds(n) => u16::try_from(*n).unwrap(),
            _ => 1,
        }
    }
}

type SymbolTable = HashMap<String, String>;

#[derive(Debug)]
struct LabelMap {
    inner: HashMap<Label, String>,
}

impl LabelMap {
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn get(&self, ln: usize, label: &Label) -> Result<&String, String> {
        self.inner
            .get(label)
            .ok_or_else(|| format!("unknown operand at line {ln}: '{}'", label.0))
    }

    pub fn insert(&mut self, label: Label, address: u16) {
        self.inner.insert(label, pad_num(address));
    }
}

fn create_symbol_table(map: LabelMap) -> SymbolTable {
    let mut symbol_table = HashMap::new();
    for (k, v) in map.inner {
        symbol_table.insert(v, k.0);
    }
    symbol_table
}

/// # Errors
///
/// Will return `Err` if there was an error
pub fn assemble(asm: &str) -> Result<(SymbolTable, String), String> {
    let lines = asm.lines();
    let parsed_instructions: Result<Vec<_>, _> = lines
        .enumerate()
        .map(|(n, s)| line_to_instruction(n, s))
        .collect();
    let (instructions, map) = first_pass(&parsed_instructions?);
    let machine_code = create_machine_code(&map, instructions)?;
    let symbol_table = create_symbol_table(map);
    Ok((symbol_table, machine_code))
}

fn line_to_instruction(ln: usize, mut line: &str) -> Result<Instruction, String> {
    use Operand::{Direct, Immediate};
    line = line.split(';').next().unwrap();
    let split_by_colon: Vec<_> = line.split(':').collect();
    let mut label = None;
    let mut opcode = None;
    let rest = if split_by_colon.len() >= 2 {
        label = Some(Label(split_by_colon[0].trim().to_ascii_lowercase()));
        split_by_colon[1..].join(":")
    } else {
        split_by_colon[0].to_string()
    };
    let opcode_parts: Vec<&str> = rest.split_whitespace().collect();
    if opcode_parts.len() == 1 {
        opcode = Some(Opcode {
            ln,
            oc: match opcode_parts[0].to_ascii_lowercase().as_str() {
                "stop" => Oc::Stop,
                "in" => Oc::In,
                "out" => Oc::Out,
                "ret" => Oc::Ret,
                "push" => Oc::Push_,
                "pop" => Oc::Pop_,
                s => return Err(format!("unrecognized opcode at line {ln}: {s}")),
            },
        });
    } else if opcode_parts.len() >= 2 {
        let operand_str = opcode_parts[1..].join(" ");
        let operand = operand_str.parse::<u16>().map_or_else(
            |_| Direct(Label(operand_str.to_ascii_lowercase())),
            Immediate,
        );
        opcode = Some(Opcode {
            ln,
            oc: match opcode_parts[0].to_ascii_lowercase().as_str() {
                "ld" => Oc::Ld(operand),
                "ldi" => Oc::Ldi(operand),
                "lda" => Oc::Lda(operand),
                "st" => Oc::St(operand),
                "sti" => Oc::Sti(operand),
                "add" => Oc::Add(operand),
                "sub" => Oc::Sub(operand),
                "mul" => Oc::Mul(operand),
                "div" => Oc::Div(operand),
                "jmp" => Oc::Jmp(operand),
                "jg" => Oc::Jg(operand),
                "jl" => Oc::Jl(operand),
                "je" => Oc::Je(operand),
                "call" => match operand_str.to_ascii_lowercase().as_str() {
                    "printinteger" => Oc::CallPrintInteger,
                    "printstring" => Oc::CallPrintString,
                    "inputinteger" => Oc::CallInputInteger,
                    "inputstring" => Oc::CallInputString,
                    _ => Oc::Call(operand),
                },
                "ldparam" => Oc::Ldparam(operand),
                "jge" => Oc::Jge(operand),
                "jle" => Oc::Jle(operand),
                "jne" => Oc::Jne(operand),
                "push" => Oc::Push(operand),
                "pop" => Oc::Pop(operand),
                "pusha" => Oc::Pusha(operand),
                "dc" => {
                    Oc::Dc(operand_str[1..operand_str.len().saturating_sub(1)].replace("\\n", "\n"))
                }
                "db" => Oc::Db(operand_str.parse().unwrap()),
                "ds" => Oc::Ds(operand_str.parse().unwrap()),
                s => return Err(format!("unrecognized operand at line {ln}: {s}")),
            },
        });
    }

    Ok(Instruction { label, opcode })
}

fn first_pass(input: &[Instruction]) -> (Vec<Opcode>, LabelMap) {
    let mut instructions = vec![];
    let mut i = 0;
    let mut stored_label = None;
    let mut map = LabelMap::new();
    let mut address = 0;
    while i < input.len() {
        let cur = &input[i];
        match (&cur.label, &cur.opcode) {
            (None, None) => {}
            (Some(label), None) => {
                stored_label = Some(label.clone());
            }
            (Some(label), Some(oc)) => {
                map.insert(label.clone(), address);
                instructions.push(oc.clone());
                address = address.saturating_add(oc.len());
            }
            (None, Some(oc)) => {
                let label = stored_label.take();
                instructions.push(oc.clone());
                if let Some(l) = label {
                    map.insert(l, address);
                }
                address = address.saturating_add(oc.len());
            }
        }
        i = i.saturating_add(1);
    }
    (instructions, map)
}

fn pad_num(n: u16) -> String {
    let mut s = n.to_string();
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

fn create_machine_code(map: &LabelMap, opcodes: Vec<Opcode>) -> Result<String, String> {
    let mut s = String::new();
    let mut address = 0;
    for opcode in opcodes {
        let new_address = opcode_to_machine(&mut s, map, address, opcode)?;
        address = new_address;
    }
    Ok(s)
}

fn opcode_to_machine(
    s: &mut String,
    map: &LabelMap,
    a: u16,
    opcode: Opcode,
) -> Result<u16, String> {
    use Operand::{Direct, Immediate};
    write!(s, "{} ", pad_num(a)).unwrap();
    let mut new_address = a.saturating_add(1);
    match opcode.oc {
        Oc::Stop => writeln!(s, "00000").unwrap(),
        Oc::Ld(Direct(label)) => writeln!(s, "01{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::Ld(Immediate(n)) => writeln!(s, "91{}", pad_num(n)).unwrap(),
        Oc::Ldi(Direct(label)) => writeln!(s, "02{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::Lda(Direct(label)) => writeln!(s, "03{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::St(Direct(label)) => writeln!(s, "04{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::Sti(Direct(label)) => writeln!(s, "05{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::Add(Direct(label)) => writeln!(s, "06{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::Add(Immediate(n)) => writeln!(s, "96{}", pad_num(n)).unwrap(),
        Oc::Sub(Direct(label)) => writeln!(s, "07{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::Sub(Immediate(n)) => writeln!(s, "97{}", pad_num(n)).unwrap(),
        Oc::Mul(Direct(label)) => writeln!(s, "08{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::Mul(Immediate(n)) => writeln!(s, "98{}", pad_num(n)).unwrap(),
        Oc::Div(Direct(label)) => writeln!(s, "09{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::Div(Immediate(n)) => writeln!(s, "99{}", pad_num(n)).unwrap(),
        Oc::In => writeln!(s, "10000").unwrap(),
        Oc::Out => writeln!(s, "11000").unwrap(),
        Oc::Jmp(Direct(label)) => writeln!(s, "12{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::Jg(Direct(label)) => writeln!(s, "13{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::Jl(Direct(label)) => writeln!(s, "14{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::Je(Direct(label)) => writeln!(s, "15{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::Call(Direct(label)) => writeln!(s, "16{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::Ret => writeln!(s, "17000").unwrap(),
        Oc::Push_ => writeln!(s, "18000").unwrap(),
        Oc::Pop_ => writeln!(s, "19000").unwrap(),
        Oc::Ldparam(Immediate(n)) => writeln!(s, "20{}", pad_num(n)).unwrap(),
        Oc::Jge(Direct(label)) => writeln!(s, "21{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::Jle(Direct(label)) => writeln!(s, "22{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::Jne(Direct(label)) => writeln!(s, "23{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::Push(Direct(label)) => writeln!(s, "24{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::Pop(Direct(label)) => writeln!(s, "25{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::Pusha(Direct(label)) => writeln!(s, "26{}", map.get(opcode.ln, &label)?).unwrap(),
        Oc::CallPrintInteger => writeln!(s, "16900").unwrap(),
        Oc::CallPrintString => writeln!(s, "16925").unwrap(),
        Oc::CallInputInteger => writeln!(s, "16950").unwrap(),
        Oc::CallInputString => writeln!(s, "16975").unwrap(),
        Oc::Db(n) => writeln!(s, "{}", pad_five(n)).unwrap(),
        Oc::Ds(n) => {
            writeln!(s, "00000").unwrap();
            let mut x = a.saturating_add(1);
            for _ in 1..n {
                writeln!(s, "{} 00000", pad_num(x)).unwrap();
                x = x.saturating_add(1);
            }
            new_address = x;
        }
        Oc::Dc(string) => {
            let cs: Vec<_> = string.chars().collect();
            writeln!(s, "{}", pad_five(cs[0] as i32)).unwrap();
            let mut x = a;
            for c in &cs[1..] {
                writeln!(s, "{} {}", pad_num(x), pad_five(*c as i32)).unwrap();
                x = x.saturating_add(1);
            }
            writeln!(s, "{} 00000", pad_num(x)).unwrap();
            new_address = x.saturating_add(1);
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
