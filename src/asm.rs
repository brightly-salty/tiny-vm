use std::collections::HashMap;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct Label(String);

#[derive(Clone)]
enum Operand {
    Direct(Label),  // must match operand
    Immediate(u16), // 0-999
}

#[derive(Clone)]
enum Opcode {
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
        match self {
            Self::Dc(s) => u16::try_from(s.chars().count()).unwrap() + 1,
            _ => 1,
        }
    }
}

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

    pub fn get(&self, label: &Label) -> &String {
        self.inner
            .get(label)
            .unwrap_or_else(|| panic!("label '{}' not found", label.0))
    }

    pub fn insert(&mut self, label: Label, address: u16) {
        self.inner.insert(label, pad_num(address));
    }
}

pub fn assemble(asm: &str) -> String {
    let lines = asm.lines();
    let parsed_instructions: Vec<_> = lines
        .enumerate()
        .map(|(n, s)| line_to_instruction(n, s))
        .collect();
    let (instructions, map) = first_pass(&parsed_instructions);
    create_machine_code(&map, instructions)
}

fn line_to_instruction(line_number: usize, mut line: &str) -> Instruction {
    use Opcode::*;
    use Operand::*;
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
        opcode = Some(match opcode_parts[0].to_ascii_lowercase().as_str() {
            "stop" => Stop,
            "in" => In,
            "out" => Out,
            "ret" => Ret,
            "push" => Push_,
            "pop" => Pop_,
            s => panic!("unrecognized opcode at line {line_number}: {s}"),
        });
    } else if opcode_parts.len() >= 2 {
        let operand_str = opcode_parts[1..].join(" ");
        let operand = operand_str.parse::<u16>().map_or_else(
            |_| Direct(Label(operand_str.to_ascii_lowercase())),
            Immediate,
        );
        opcode = Some(match opcode_parts[0].to_ascii_lowercase().as_str() {
            "ld" => Ld(operand),
            "ldi" => Ldi(operand),
            "lda" => Lda(operand),
            "st" => St(operand),
            "sti" => Sti(operand),
            "add" => Add(operand),
            "sub" => Sub(operand),
            "mul" => Mul(operand),
            "div" => Div(operand),
            "jmp" => Jmp(operand),
            "jg" => Jg(operand),
            "jl" => Jl(operand),
            "je" => Je(operand),
            "call" => match operand_str.to_ascii_lowercase().as_str() {
                "printinteger" => CallPrintInteger,
                "printstring" => CallPrintString,
                "inputinteger" => CallInputInteger,
                "inputstring" => CallInputString,
                _ => Call(operand),
            },
            "ldparam" => Ldparam(operand),
            "jge" => Jge(operand),
            "jle" => Jle(operand),
            "jne" => Jne(operand),
            "push" => Push(operand),
            "pop" => Pop(operand),
            "pusha" => Pusha(operand),
            "dc" => Dc(operand_str[1..operand_str.len() - 1].replace("\\n", "\n")),
            "db" => Db(operand_str.parse().unwrap()),
            "ds" => Ds(operand_str.parse().unwrap()),
            _ => panic!("unrecognized operand"),
        });
    }

    Instruction { label, opcode }
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
                address += oc.len();
            }
            (None, Some(oc)) => {
                let label = stored_label.take();
                instructions.push(oc.clone());
                if let Some(l) = label {
                    map.insert(l, address);
                }
                address += oc.len();
            }
        }
        i += 1;
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

fn create_machine_code(map: &LabelMap, opcodes: Vec<Opcode>) -> String {
    let mut s = String::new();
    let mut address = 0;
    for opcode in opcodes {
        let (new_address, string) = opcode_to_machine(map, address, opcode);
        s.push_str(&string);
        address = new_address;
    }
    s
}

fn opcode_to_machine(map: &LabelMap, a: u16, opcode: Opcode) -> (u16, String) {
    use Opcode::*;
    use Operand::*;
    let mut new_address = a + 1;
    let new_string = match opcode {
        Stop => format!("{} 00000\n", pad_num(a)),
        Ld(Direct(label)) => format!("{} 01{}\n", pad_num(a), map.get(&label)),
        Ld(Immediate(n)) => format!("{} 91{}\n", pad_num(a), pad_num(n)),
        Ldi(Direct(label)) => format!("{} 02{}\n", pad_num(a), map.get(&label)),
        Lda(Direct(label)) => format!("{} 03{}\n", pad_num(a), map.get(&label)),
        St(Direct(label)) => format!("{} 04{}\n", pad_num(a), map.get(&label)),
        Sti(Direct(label)) => format!("{} 05{}\n", pad_num(a), map.get(&label)),
        Add(Direct(label)) => format!("{} 06{}\n", pad_num(a), map.get(&label)),
        Add(Immediate(n)) => format!("{} 96{}\n", pad_num(a), pad_num(n)),
        Sub(Direct(label)) => format!("{} 07{}\n", pad_num(a), map.get(&label)),
        Sub(Immediate(n)) => format!("{} 97{}\n", pad_num(a), pad_num(n)),
        Mul(Direct(label)) => format!("{} 08{}\n", pad_num(a), map.get(&label)),
        Mul(Immediate(n)) => format!("{} 98{}\n", pad_num(a), pad_num(n)),
        Div(Direct(label)) => format!("{} 09{}\n", pad_num(a), map.get(&label)),
        Div(Immediate(n)) => format!("{} 99{}\n", pad_num(a), pad_num(n)),
        In => format!("{} 10000\n", pad_num(a)),
        Out => format!("{} 11000\n", pad_num(a)),
        Jmp(Direct(label)) => format!("{} 12{}\n", pad_num(a), map.get(&label)),
        Jg(Direct(label)) => format!("{} 13{}\n", pad_num(a), map.get(&label)),
        Jl(Direct(label)) => format!("{} 14{}\n", pad_num(a), map.get(&label)),
        Je(Direct(label)) => format!("{} 15{}\n", pad_num(a), map.get(&label)),
        Call(Direct(label)) => format!("{} 16{}\n", pad_num(a), map.get(&label)),
        Ret => format!("{} 17000\n", pad_num(a)),
        Push_ => format!("{} 18000\n", pad_num(a)),
        Pop_ => format!("{} 19000\n", pad_num(a)),
        Ldparam(Immediate(n)) => format!("{} 20{}\n", pad_num(a), pad_num(n)),
        Jge(Direct(label)) => format!("{} 21{}\n", pad_num(a), map.get(&label)),
        Jle(Direct(label)) => format!("{} 22{}\n", pad_num(a), map.get(&label)),
        Jne(Direct(label)) => format!("{} 23{}\n", pad_num(a), map.get(&label)),
        Push(Direct(label)) => format!("{} 24{}\n", pad_num(a), map.get(&label)),
        Pop(Direct(label)) => format!("{} 25{}\n", pad_num(a), map.get(&label)),
        Pusha(Direct(label)) => format!("{} 26{}\n", pad_num(a), map.get(&label)),
        CallPrintInteger => format!("{} 16900\n", pad_num(a)),
        CallPrintString => format!("{} 16925\n", pad_num(a)),
        CallInputInteger => format!("{} 16950\n", pad_num(a)),
        CallInputString => format!("{} 16975\n", pad_num(a)),
        Db(n) => format!("{} {}\n", pad_num(a), pad_five(n)),
        Ds(n) => {
            let mut s = String::new();
            let mut x = a;
            for _ in 0..n {
                s.push_str(&format!("{} 00000\n", pad_num(x)));
                x += 1;
            }
            new_address = x;
            s
        }
        Dc(string) => {
            let mut s = String::new();
            let mut x = a;
            for c in string.chars() {
                s.push_str(&format!("{} {} {c}\n", pad_num(x), pad_five(c as i32)));
                x += 1;
            }
            s.push_str(&format!("{} 00000\n", pad_num(x)));
            new_address = x + 1;
            s
        }
        _ => panic!(),
    };
    (new_address, new_string)
}
