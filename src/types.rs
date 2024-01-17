use std::fmt;
use rand::rngs::ThreadRng;
use rand::Rng;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Hash)]
pub struct Address(pub u16); //0-999 (3 decimal digits)

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

impl fmt::Display for Address {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:0>3}", self.0.to_string())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct Byte(pub i32); //-99999-99999 (5 decimal digits plus sign)

impl Byte {
    /// # Errors
    ///
    /// Will return `Err` if the byte is not a valid instruction
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

    /// # Errors
    ///
    /// Will return `Err` if the byte is not a valid address
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

    /// # Errors
    ///
    /// Will return `Err` if the byte is not a valid ASCII char
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

    pub fn random(rng: &mut ThreadRng) -> Self {
        Self(rng.gen_range(0..10_000))
    }
}

impl fmt::Display for Byte {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:0>5}", self.0.to_string())
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct Opcode(pub u8); //0-99 (2 decimal digits)

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operand: Address,
}

impl Instruction {
    #[must_use]
    pub const fn as_byte(&self) -> Byte {
        Byte((self.opcode.0 as i32).saturating_mul(1_000).saturating_add(self.operand.0 as i32))
    }
}