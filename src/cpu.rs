use console::Term;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct Address(u16); //0-999 (3 decimal digits)

impl Address {
    #[must_use]
    pub fn from_instruction(n: u32) -> Self {
        match u16::try_from(n % 1000) {
            Ok(n) if n < 1000 => Self(n),
            _ => panic!("Operand of instruction {n} was invalid"),
        }
    }

    #[must_use]
    pub fn from_byte(b: Byte) -> Self {
        match u16::try_from(b.0) {
            Ok(n) if n < 1000 => Self(n),
            _ => panic!("Byte {} could not be read as address", b.0),
        }
    }

    #[must_use]
    pub fn new(b: u16) -> Self {
        debug_assert!(b < 1000);
        Self(b)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct Byte(i32); //-99999-99999 (5 decimal digits plus sign)

impl Byte {
    #[must_use]
    pub fn from_address(a: Address) -> Self {
        Self(i32::from(a.0))
    }

    #[must_use]
    pub fn from_char(c: char) -> Self {
        Self(i32::from(c as u8))
    }

    #[must_use]
    pub fn to_char(self) -> char {
        u8::try_from(self.0).map_or_else(|_| panic!("Byte {} could not be read as ASCII char", self.0), char::from)
    }

    #[must_use]
    pub fn new(b: i32) -> Self {
        debug_assert!(b < 100_000 && b > -100_000);
        Self(b)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct Opcode(u8); //0-99 (2 decimal digits)

impl Opcode {
    #[must_use]
    pub fn from_instruction(n: u32) -> Self {
        match u8::try_from(n / 1000) {
            Ok(n) if n < 100 => Self(n),
            _ => panic!("Opcode of instruction {n} was invalid"),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct Instruction {
    opcode: Opcode,
    operand: Address,
}

impl Instruction {
    #[must_use]
    pub fn from_byte(b: Byte) -> Self {
        match u32::try_from(b.0) {
            Ok(n) if n < 100_000 => Self {
                opcode: Opcode::from_instruction(n),
                operand: Address::from_instruction(n),
            },
            _ => panic!("Byte {} could not be read as instruction", b.0),
        }
    }
}

pub struct Memory([Byte; 1000]);

impl Default for Memory {
    fn default() -> Self {
        Self([Byte(0); 1000])
    }
}

impl Memory {
    #[must_use]
    pub const fn get_contents(&self, addr: Address) -> Byte {
        self.0[addr.0 as usize]
    }

    pub fn set_contents(&mut self, addr: Address, b: Byte) {
        self.0[addr.0 as usize] = b;
    }
}

#[derive(Default)]
pub struct Cu {
    ip: Address,
    ir: Instruction,
}

#[derive(Default)]
pub struct Alu {
    acc: Byte,
    sp: Address,
    bp: Address,
}

#[derive(Default)]
pub struct Cpu {
    cu: Cu,
    alu: Alu,
    memory: Memory,
}

impl Cpu {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// # Panics
    ///
    /// Will panic if an invariant is encountered
    #[allow(clippy::too_many_lines)]
    pub fn run(&mut self) {
        loop {
            self.cu.ir = Instruction::from_byte(self.c(self.cu.ip));
            self.cu.ip.0 += 1;
            let adr = self.cu.ir.operand;
            match self.cu.ir.opcode.0 {
                0 => break,
                1 => {
                    self.alu.acc = self.c(adr);
                }
                91 | 3 => {
                    self.alu.acc = self.imm();
                }
                2 => {
                    self.alu.acc = self.c_c(adr);
                }
                4 => {
                    self.memory.set_contents(self.cu.ir.operand, self.alu.acc);
                }
                5 => {
                    self.set_c(Address::from_byte(self.c(adr)), self.alu.acc);
                }
                6 => {
                    self.alu.acc.0 += self.c(adr).0;
                }
                96 => {
                    self.alu.acc.0 += self.imm().0;
                }
                7 => {
                    self.alu.acc.0 -= self.c(adr).0;
                }
                97 => {
                    self.alu.acc.0 -= self.imm().0;
                }
                8 => {
                    self.alu.acc.0 *= self.c(adr).0;
                }
                98 => {
                    self.alu.acc.0 *= self.imm().0;
                }
                9 => {
                    self.alu.acc.0 /= self.c(adr).0;
                }
                99 => {
                    self.alu.acc.0 /= self.imm().0;
                }
                10 => {
                    self.alu.acc = Byte::from_char(Term::stdout().read_char().unwrap());
                }
                11 => {
                    print!("{}", self.alu.acc.to_char());
                }
                12 => {
                    self.cu.ip = adr;
                }
                13 => {
                    if self.alu.acc.0 > 0 {
                        self.cu.ip = adr;
                    }
                }
                14 => {
                    if self.alu.acc.0 < 1 {
                        self.cu.ip = adr;
                    }
                }
                15 => {
                    if self.alu.acc.0 == 0 {
                        self.cu.ip = adr;
                    }
                }
                16 => match self.cu.ir.operand {
                    Address(900) => self.print_integer(),
                    Address(925) => self.print_string(),
                    Address(950) => self.input_integer(),
                    Address(975) => self.input_string(),
                    _ => {
                        self.alu.sp.0 -= 1;
                        self.memory
                            .set_contents(self.alu.sp, Byte::from_address(self.cu.ip));
                        self.alu.sp.0 -= 1;
                        self.memory
                            .set_contents(self.alu.sp, Byte::from_address(self.alu.bp));
                        self.alu.bp = self.alu.sp;
                        self.cu.ip = adr;
                    }
                },
                17 => {
                    self.alu.sp = self.alu.bp;
                    self.alu.bp = Address::from_byte(self.c(self.alu.sp));
                    self.alu.sp.0 += 1;
                    self.cu.ip = Address::from_byte(self.c(self.alu.sp));
                    self.alu.sp.0 += 1;
                }
                18 => {
                    self.alu.sp.0 -= 1;
                    self.set_c(self.alu.sp, self.alu.acc);
                }
                19 => {
                    self.alu.acc = self.c(self.alu.sp);
                    self.alu.sp.0 += 1;
                }
                20 => {
                    self.alu.acc = self.c(Address(self.alu.bp.0 + self.cu.ir.operand.0 + 1));
                }
                21 => {
                    if self.alu.acc.0 >= 0 {
                        self.cu.ip = adr;
                    }
                }
                22 => {
                    if self.alu.acc.0 <= 1 {
                        self.cu.ip = adr;
                    }
                }
                23 => {
                    if self.alu.acc.0 != 0 {
                        self.cu.ip = adr;
                    }
                }
                24 => {
                    self.alu.sp.0 -= 1;
                    self.set_c(self.alu.sp, self.c(adr));
                }
                25 => {
                    self.set_c(adr, self.c(self.alu.sp));
                    self.alu.sp.0 += 1;
                }
                26 => {
                    self.alu.sp.0 -= 1;
                    self.set_c(self.alu.sp, Byte::from_address(adr));
                }
                _ => panic!("unrecognized opcode"),
            }
        }
    }

    const fn c(&self, adr: Address) -> Byte {
        self.memory.get_contents(adr)
    }

    fn c_c(&self, adr: Address) -> Byte {
        self.c(Address::from_byte(self.c(adr)))
    }

    fn set_c(&mut self, adr: Address, b: Byte) {
        self.memory.set_contents(Address::from_byte(self.c(adr)), b);
    }

    fn imm(&self) -> Byte {
        Byte::from_address(self.cu.ir.operand)
    }

    fn print_integer(&self) {
        print!("{}", self.alu.acc.0);
    }

    fn print_string(&self) {
        let mut addr = Address::from_byte(self.alu.acc);
        let mut value = self.c(addr);
        while value != Byte(0) {
            print!("{}", value.to_char());
            addr.0 += 1;
            value = self.c(addr);
        }
    }

    fn input_integer(&mut self) {
        self.alu.acc = Byte(Term::stdout().read_line().unwrap().parse().unwrap());
    }

    fn input_string(&mut self) {
        let mut addr = Address::from_byte(self.alu.acc);
        for c in Term::stdout().read_line().unwrap().chars() {
            self.memory.set_contents(addr, Byte::from_char(c));
            addr.0 += 1;
        }
        self.memory.set_contents(addr, Byte(0));
    }

    pub fn parse_machine_code(&mut self, s: &str) {
        for line in s.lines() {
            let words: Vec<_> = line.split_whitespace().collect();
            if words.len() >= 2 {
                let address = Address::new(words[0].parse().unwrap());
                let instruction = Byte::new(words[1].parse().unwrap());
                self.memory.set_contents(address, instruction);
            }
        }
    }
}
