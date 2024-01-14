use rand::Rng;
use std::io::Read;
use std::io::Write;
use std::ops::{Index, IndexMut};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct Address(u16); //0-999 (3 decimal digits)

impl Address {
    #[must_use]
    pub fn new(b: u16) -> Self {
        debug_assert!(b < 1000);
        Self(b)
    }

    #[must_use]
    pub fn into_byte(self) -> Byte {
        Byte(i32::from(self.0))
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct Byte(i32); //-99999-99999 (5 decimal digits plus sign)

impl Byte {
    pub fn read_as_instruction(self) -> Result<Instruction, String> {
        match u32::try_from(self.0) {
            Ok(n) if n < 100_000 => Ok(Instruction {
                opcode: u8::try_from(n / 1000)
                    .map(Opcode)
                    .map_err(|_| format!("Opcode of instruction {n} was invalid"))?,
                operand: u16::try_from(n % 1000)
                    .map(Address)
                    .map_err(|_| format!("Operand of instruction {n} was invalid"))?,
            }),
            _ => Err(format!("Byte {} could not be read as instruction", self.0)),
        }
    }

    pub fn read_as_address(self) -> Result<Address, String> {
        match u16::try_from(self.0) {
            Ok(n) if n < 1000 => Ok(Address(n)),
            _ => Err(format!("Byte {} could not be read as address", self.0)),
        }
    }

    #[must_use]
    pub fn from_char(c: char) -> Self {
        Self(i32::from(c as u8))
    }

    pub fn read_as_char(self) -> Result<char, String> {
        u8::try_from(self.0)
            .map(char::from)
            .map_err(|_| format!("Byte {} could not be read as ASCII char", self.0))
    }

    #[must_use]
    pub fn new(b: i32) -> Self {
        debug_assert!(b < 100_000 && b > -100_000);
        Self(b)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct Opcode(u8); //0-99 (2 decimal digits)

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct Instruction {
    opcode: Opcode,
    operand: Address,
}

#[derive(Clone, Copy)]
pub struct Memory([Byte; 1000]);

impl Memory {
    fn new() -> Self {
        let mut rng = rand::thread_rng();
        let mut buffer = [Byte(0); 1000];
        for n in &mut buffer {
            *n = Byte(rng.gen_range(0..10_000));
        }
        Self(buffer)
    }
}

impl Index<Address> for Memory {
    type Output = Byte;

    fn index(&self, index: Address) -> &Byte {
        &self.0[index.0 as usize]
    }
}

impl IndexMut<Address> for Memory {
    fn index_mut(&mut self, index: Address) -> &mut Byte {
        &mut self.0[index.0 as usize]
    }
}

#[derive(Clone, Default)]
pub struct Cu {
    ip: Address,
    ir: Instruction,
}

#[derive(Clone)]
pub struct Alu {
    acc: Byte,
    sp: Address,
    bp: Address,
}

impl Alu {
    const fn new() -> Self {
        Self {
            acc: Byte(0),
            sp: Address(900),
            bp: Address(900),
        }
    }
}

#[derive(Clone)]
pub struct Cpu {
    cu: Cu,
    alu: Alu,
    memory: Memory,
}

impl Default for Cpu {
    fn default() -> Self {
        Self {
            cu: Cu::default(),
            alu: Alu::new(),
            memory: Memory::new(),
        }
    }
}

impl Cpu {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// # Errors
    ///
    /// Will return `Err` if there was an unrecoverable error while running the CPU
    pub fn run(&mut self) -> Result<(), String> {
        loop {
            let should_continue = self.step()?;
            if !should_continue {
                break;
            }
        }
        Ok(())
    }

    /// # Errors
    ///
    /// Will return `Err` if there was an unknown opcode
    #[allow(clippy::too_many_lines)]
    pub fn step(&mut self) -> Result<bool, String> {
        self.cu.ir = self.memory[self.cu.ip].read_as_instruction()?;
        self.cu.ip.0 = self.cu.ip.0.saturating_add(1);
        let adr = self.cu.ir.operand;
        let c_adr = || self.memory[adr];
        let imm = || self.cu.ir.operand.into_byte();
        match self.cu.ir.opcode.0 {
            0 => return Ok(false),
            1 => {
                self.load_into_acc(c_adr());
            }
            91 | 3 => {
                self.load_into_acc(imm());
            }
            2 => {
                self.load_into_acc(self.memory[c_adr().read_as_address()?]);
            }
            4 => {
                self.store_acc_into(adr);
            }
            5 => {
                self.store_acc_into(c_adr().read_as_address()?);
            }
            6 => {
                self.add_to_acc(c_adr());
            }
            96 => {
                self.add_to_acc(imm());
            }
            7 => {
                self.sub_from_acc(c_adr());
            }
            97 => {
                self.sub_from_acc(imm());
            }
            8 => {
                self.mul_acc_by(c_adr());
            }
            98 => {
                self.mul_acc_by(imm());
            }
            9 => {
                self.div_acc_by(c_adr())?;
            }
            99 => {
                self.div_acc_by(imm())?;
            }
            10 => {
                let mut stdin = std::io::stdin();
                let mut buf = [0; 2];
                stdin
                    .read_exact(&mut buf)
                    .map_err(|_| "Could not read a character from stdin".to_owned())?;
                self.load_into_acc(Byte::from_char(char::from(buf[0])));
            }
            11 => {
                print!("{}", self.alu.acc.read_as_char()?);
            }
            12 => {
                self.jump_to(adr);
            }
            13 => {
                if self.alu.acc.0 > 0 {
                    self.jump_to(adr);
                }
            }
            14 => {
                if self.alu.acc.0 < 1 {
                    self.jump_to(adr);
                }
            }
            15 => {
                if self.alu.acc.0 == 0 {
                    self.jump_to(adr);
                }
            }
            16 => match self.cu.ir.operand {
                Address(900) => self.print_integer(),
                Address(925) => self.print_string()?,
                Address(950) => self.input_integer(),
                Address(975) => self.input_string()?,
                _ => {
                    // Push a call-frame onto the stack
                    self.push(self.cu.ip.into_byte())?; // push return address
                    self.push(self.alu.bp.into_byte())?; // push previous value of BP
                    self.alu.bp = self.alu.sp; // update value of BF
                    self.jump_to(adr); // jump to start of function
                }
            },
            17 => {
                self.alu.sp = self.alu.bp; // pop call-frame off the stack
                self.alu.bp = self.pop().read_as_address()?; // restore BP to its previous value
                let address = self.pop().read_as_address()?;
                self.jump_to(address); // restore IP to return address
            }
            18 => {
                self.push(self.alu.acc)?;
            }
            19 => {
                let byte = self.pop();
                self.load_into_acc(byte);
            }
            20 => {
                self.load_into_acc(
                    self.memory[Address(
                        self.alu
                            .bp
                            .0
                            .saturating_add(self.cu.ir.operand.0)
                            .saturating_add(1),
                    )],
                );
            }
            21 => {
                if self.alu.acc.0 >= 0 {
                    self.jump_to(adr);
                }
            }
            22 => {
                if self.alu.acc.0 <= 1 {
                    self.jump_to(adr);
                }
            }
            23 => {
                if self.alu.acc.0 != 0 {
                    self.jump_to(adr);
                }
            }
            24 => {
                self.push(c_adr())?;
            }
            25 => {
                let byte = self.pop();
                let address = self.memory[adr];
                self.memory[address.read_as_address()?] = byte;
            }
            26 => {
                self.push(adr.into_byte())?;
            }
            x => return Err(format!("unrecognized opcode {x}")),
        }
        Ok(true)
    }

    fn load_into_acc(&mut self, b: Byte) {
        self.alu.acc = b;
    }

    fn store_acc_into(&mut self, address: Address) {
        self.memory[address] = self.alu.acc;
    }

    fn add_to_acc(&mut self, b: Byte) {
        self.alu.acc.0 = self.alu.acc.0.saturating_add(b.0);
    }

    fn sub_from_acc(&mut self, b: Byte) {
        self.alu.acc.0 = self.alu.acc.0.saturating_sub(b.0);
    }

    fn mul_acc_by(&mut self, b: Byte) {
        self.alu.acc.0 = self.alu.acc.0.saturating_mul(b.0);
    }

    fn div_acc_by(&mut self, b: Byte) -> Result<(), String> {
        self.alu.acc.0 = self
            .alu
            .acc
            .0
            .checked_div(b.0)
            .ok_or_else(|| "Divided by 0 or integer underflow".to_owned())?;
        Ok(())
    }

    fn push(&mut self, byte: Byte) -> Result<(), String> {
        self.alu.sp.0 = self.alu.sp.0.saturating_sub(1);
        let address = self.memory[self.alu.sp].read_as_address()?;
        self.memory[address] = byte;
        Ok(())
    }

    fn jump_to(&mut self, adr: Address) {
        self.cu.ip = adr;
    }

    #[must_use]
    fn pop(&mut self) -> Byte {
        let byte = self.memory[self.alu.sp];
        self.alu.sp.0 = self.alu.sp.0.saturating_add(1);
        byte
    }

    fn print_integer(&self) {
        println!("{}", self.alu.acc.0);
        let mut stdout = std::io::stdout();
        stdout.flush().unwrap();
    }

    fn print_string(&self) -> Result<(), String> {
        let mut addr = self.alu.acc.read_as_address()?;
        let mut buffer = String::new();
        let mut value = self.memory[addr];
        while value != Byte(0) {
            buffer.push(value.read_as_char()?);
            addr.0 = addr.0.saturating_add(1);
            value = self.memory[addr];
        }
        print!("{buffer}");
        std::io::stdout()
            .flush()
            .map_err(|_| "Could not flush stdout".to_owned())?;
        Ok(())
    }

    fn input_integer(&mut self) {
        let stdin = std::io::stdin();
        let mut buffer = String::new();
        stdin.read_line(&mut buffer).unwrap();
        self.load_into_acc(Byte(buffer.trim().parse().unwrap()));
    }

    fn input_string(&mut self) -> Result<(), String> {
        let stdin = std::io::stdin();
        let mut buffer = String::new();
        stdin
            .read_line(&mut buffer)
            .map_err(|_| "Could not read line from stdin")?;
        let mut addr = self.alu.acc.read_as_address()?;
        for c in buffer.trim().chars() {
            self.memory[addr] = Byte::from_char(c);
            addr.0 = addr.0.saturating_add(1);
        }
        self.memory[addr] = Byte(0);
        Ok(())
    }

    /// # Panics
    ///
    /// Panics if the machine code is malformed
    pub fn parse_machine_code(&mut self, s: &str) {
        for line in s.lines() {
            let words: Vec<_> = line.split_whitespace().collect();
            if words.len() >= 2 {
                self.memory[Address::new(words[0].parse().unwrap())] =
                    Byte::new(words[1].parse().unwrap());
            }
        }
    }
}
