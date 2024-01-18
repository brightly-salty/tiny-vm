use crate::types::{Address, Byte, Instruction, TinyError, TinyResult};
use std::io::Read;
use std::ops::{Index, IndexMut};

#[derive(Clone, Copy)]
pub struct Memory([Byte; 1000]);

impl Memory {
    fn new() -> Self {
        let mut rng = rand::thread_rng();
        let mut buffer = [Byte(0); 1000];
        for n in &mut buffer {
            *n = Byte::random(&mut rng);
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
    pub ip: Address,
    pub ir: Instruction,
}

#[derive(Clone)]
pub struct Alu {
    pub acc: Byte,
    pub sp: Address,
    pub bp: Address,
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

#[derive(Clone, Copy)]
enum State {
    WaitingForInput,
    Stopped,
    ReadyToCycle,
}

#[derive(Clone)]
pub enum Input {
    Char(char),
    Integer(i32),
    String(String),
    None,
}

#[derive(Clone)]
pub enum Output {
    WaitingForChar,
    WaitingForInteger,
    WaitingForString,
    Char(char),
    String(String),
    Stopped,
    ReadyToCycle,
}

#[derive(Clone)]
pub struct Cpu {
    pub cu: Cu,
    pub alu: Alu,
    pub memory: Memory,
    state: State,
}

impl Default for Cpu {
    fn default() -> Self {
        Self {
            cu: Cu::default(),
            alu: Alu::new(),
            memory: Memory::new(),
            state: State::ReadyToCycle,
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
    pub fn run(&mut self) -> TinyResult<()> {
        let mut input = Input::None;
        loop {
            match self.step(input)? {
                Output::WaitingForChar => {
                    let mut buffer = [0; 2];
                    std::io::stdin()
                        .read_exact(&mut buffer)
                        .map_err(|_| TinyError::InputError("an ASCII character".to_string()))?;
                    input = Input::Char(buffer[0] as char);
                }
                Output::WaitingForInteger => {
                    let mut buffer = String::new();
                    std::io::stdin()
                        .read_line(&mut buffer)
                        .map_err(|_| TinyError::InputError("an integer".to_string()))?;
                    input = Input::Integer(
                        buffer
                            .trim()
                            .parse()
                            .map_err(|_| TinyError::InputError("an integer".to_string()))?,
                    );
                }
                Output::WaitingForString => {
                    let mut buffer = String::new();
                    std::io::stdin()
                        .read_line(&mut buffer)
                        .map_err(|_| TinyError::InputError("a string".to_string()))?;
                    input = Input::String(buffer.trim().to_owned());
                }
                Output::Char(c) => {
                    print!("{c}");
                    input = Input::None;
                }
                Output::String(s) => {
                    println!("{s}");
                    input = Input::None;
                }
                Output::ReadyToCycle => {
                    input = Input::None;
                }
                Output::Stopped => {
                    break;
                }
            }
        }
        Ok(())
    }

    /// # Errors
    ///
    /// Will return `Err` if the wrong type of input is received or an unknown opcode is given
    pub fn step(&mut self, input: Input) -> TinyResult<Output> {
        match (input, self.state) {
            (Input::None, State::ReadyToCycle) => self.cycle(),
            (Input::None, State::Stopped) => Ok(Output::Stopped),
            (Input::String(s), State::WaitingForInput) => {
                let mut addr = self.alu.acc.read_as_address()?;
                for c in s.trim().chars() {
                    self.memory[addr] = Byte::from_char(c);
                    addr.0 = addr.0.saturating_add(1);
                }
                self.memory[addr] = Byte(0);
                self.state = State::ReadyToCycle;
                Ok(Output::ReadyToCycle)
            }
            (Input::Integer(i), State::WaitingForInput) => {
                self.load_into_acc(Byte(i));
                self.state = State::ReadyToCycle;
                Ok(Output::ReadyToCycle)
            }
            (Input::Char(c), State::WaitingForInput) => {
                self.load_into_acc(Byte::from_char(c));
                self.state = State::ReadyToCycle;
                Ok(Output::ReadyToCycle)
            }
            (_, _) => Err(TinyError::InternalProgramError(
                "input type did not match expectation".to_string(),
            )),
        }
    }

    #[allow(clippy::too_many_lines)]
    fn cycle(&mut self) -> TinyResult<Output> {
        self.cu.ir = self.memory[self.cu.ip].read_as_instruction()?;
        self.cu.ip.0 = self.cu.ip.0.saturating_add(1);
        let adr = self.cu.ir.operand;
        let c_adr = || self.memory[adr];
        let imm = || self.cu.ir.operand.into_byte();
        match self.cu.ir.opcode.0 {
            0 => {
                self.state = State::Stopped;
                return Ok(Output::Stopped);
            }
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
                self.add_to_acc(c_adr())?;
            }
            96 => {
                self.add_to_acc(imm())?;
            }
            7 => {
                self.sub_from_acc(c_adr())?;
            }
            97 => {
                self.sub_from_acc(imm())?;
            }
            8 => {
                self.mul_acc_by(c_adr())?;
            }
            98 => {
                self.mul_acc_by(imm())?;
            }
            9 => {
                self.div_acc_by(c_adr())?;
            }
            99 => {
                self.div_acc_by(imm())?;
            }
            10 => {
                self.state = State::WaitingForInput;
                return Ok(Output::WaitingForChar);
            }
            11 => return Ok(Output::Char(self.alu.acc.read_as_char()?)),
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
                Address(900) => return Ok(Output::String(self.alu.acc.0.to_string())),
                Address(925) => {
                    let mut address = self.alu.acc.read_as_address()?;
                    let mut buffer = String::new();
                    let mut value = self.memory[address];
                    while value != Byte(0) {
                        buffer.push(value.read_as_char()?);
                        address.0 = address.0.saturating_add(1);
                        value = self.memory[address];
                    }
                    return Ok(Output::String(buffer));
                }
                Address(950) => {
                    self.state = State::WaitingForInput;
                    return Ok(Output::WaitingForInteger);
                }
                Address(975) => {
                    self.state = State::WaitingForInput;
                    return Ok(Output::WaitingForString);
                }
                _ => {
                    // Push a call-frame onto the stack
                    self.push(self.cu.ip.into_byte()); // push return address
                    self.push(self.alu.bp.into_byte()); // push previous value of BP
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
                self.push(self.alu.acc);
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
                self.push(c_adr());
            }
            25 => {
                let byte = self.pop();
                let address = self.memory[adr];
                self.memory[address.read_as_address()?] = byte;
            }
            26 => {
                self.push(adr.into_byte());
            }
            x => {
                return Err(TinyError::InternalProgramError(format!(
                    "machine code generated by assembler contains the invalid opcode {x}"
                )))
            }
        }
        Ok(Output::ReadyToCycle)
    }

    fn load_into_acc(&mut self, b: Byte) {
        self.alu.acc = b;
    }

    fn store_acc_into(&mut self, address: Address) {
        self.memory[address] = self.alu.acc;
    }

    fn add_to_acc(&mut self, b: Byte) -> TinyResult<()> {
        self.alu.acc.0 = self.alu.acc.0.checked_add(b.0).ok_or_else(|| {
            TinyError::ArithmeticError(self.alu.acc.to_string(), '+', b.to_string())
        })?;
        Ok(())
    }

    fn sub_from_acc(&mut self, b: Byte) -> TinyResult<()> {
        self.alu.acc.0 = self.alu.acc.0.checked_sub(b.0).ok_or_else(|| {
            TinyError::ArithmeticError(self.alu.acc.to_string(), '-', b.to_string())
        })?;
        Ok(())
    }

    fn mul_acc_by(&mut self, b: Byte) -> TinyResult<()> {
        self.alu.acc.0 = self.alu.acc.0.checked_mul(b.0).ok_or_else(|| {
            TinyError::ArithmeticError(self.alu.acc.to_string(), '*', b.to_string())
        })?;
        Ok(())
    }

    fn div_acc_by(&mut self, b: Byte) -> TinyResult<()> {
        self.alu.acc.0 = self.alu.acc.0.checked_div(b.0).ok_or_else(|| {
            TinyError::ArithmeticError(self.alu.acc.to_string(), '/', b.to_string())
        })?;
        Ok(())
    }

    fn push(&mut self, byte: Byte) {
        self.alu.sp.0 = self.alu.sp.0.saturating_sub(1);
        let address = self.alu.sp;
        self.memory[address] = byte;
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

    #[must_use]
    pub fn get_stack(&self) -> Vec<Byte> {
        let mut buffer = Vec::new();
        for x in self.alu.sp.0..self.alu.bp.0 {
            buffer.push(self.memory[Address(x)]);
        }
        buffer
    }

    /// # Panics
    ///
    /// Panics if the machine code is malformed
    pub fn set_memory(&mut self, v: &[Byte]) {
        for (a, byte) in v.iter().enumerate() {
            self.memory[Address(u16::try_from(a).unwrap())] = *byte;
        }
    }
}
