use rand::rngs::ThreadRng;
use rand::Rng;
use std::fmt;

type Ln = usize;

#[derive(Debug, Clone)]
pub enum TinyError {
    UnknownOperand(Ln, String),
    UnknownOpcode(Ln, String),
    InvalidAssembly(Ln),
    InvalidEscape(Ln, char),
    InvalidOperand(Ln, String, String),
    InvalidInstruction(String),
    InvalidAddress(String),
    InvalidAscii(String),
    InternalProgramError(String),
    MismatchedDelimiter(Ln),
    InputError(String),
    OutputError,
    ArithmeticError(String, char, String),
}

impl fmt::Display for TinyError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UnknownOperand(ln, operand) => write!(f, "line {ln}: unknown operand {operand}"),
            Self::InvalidOperand(ln, opcode, operand) => write!(
                f,
                "line {ln}: invalid operand {operand} for opcode {opcode}"
            ),

            Self::UnknownOpcode(ln, opcode) => write!(f, "line {ln}: unknown opcode {opcode}"),
            Self::InvalidAssembly(ln) => {
                write!(f, "line {ln}: invalid combination of opcode and operand")
            }
            Self::MismatchedDelimiter(ln) => write!(f, "line {ln}: mismatched string delimiters"),
            Self::InvalidEscape(ln, c) => write!(f, "line {ln}: invalid escape sequence '\\{c}'"),
            Self::InvalidInstruction(s) => write!(f, "byte {s} is invalid as an instruction"),
            Self::InvalidAddress(s) => write!(f, "byte {s} is invalid as an address"),
            Self::InvalidAscii(s) => write!(f, "byte {s} is invalid as an ASCII character"),
            Self::InternalProgramError(s) => write!(f, "internal program error: {s}"),
            Self::InputError(s) => write!(f, "could not read {s} from stdin"),
            Self::OutputError => write!(f, "could not output to stdout"),
            Self::ArithmeticError(operand1, op, operand2) => write!(
                f,
                "arithmetic error when executing {operand1} {op} {operand2}"
            ),
        }
    }
}

pub type TinyResult<T> = Result<T, TinyError>;

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
    pub fn read_as_instruction(self) -> TinyResult<Instruction> {
        match u32::try_from(self.0) {
            Ok(n) if n < 100_000 => Ok(Instruction {
                opcode: u8::try_from(n / 1000).map(Opcode).map_err(|_| {
                    TinyError::InternalProgramError(format!("u32 {n} / 1000 could not fit in u8"))
                })?,
                operand: u16::try_from(n % 1000).map(Address).map_err(|_| {
                    TinyError::InternalProgramError(format!("u32 {n} % 1000 could not fit in u16"))
                })?,
            }),
            _ => Err(TinyError::InvalidInstruction(self.to_string())),
        }
    }

    /// # Errors
    ///
    /// Will return `Err` if the byte is not a valid address
    pub fn read_as_address(self) -> TinyResult<Address> {
        match u16::try_from(self.0) {
            Ok(n) if n < 1000 => Ok(Address(n)),
            _ => Err(TinyError::InvalidAddress(self.to_string())),
        }
    }

    #[must_use]
    pub fn from_char(c: char) -> Self {
        Self(i32::from(c as u8))
    }

    /// # Errors
    ///
    /// Will return `Err` if the byte is not a valid ASCII char
    pub fn read_as_char(self) -> TinyResult<char> {
        u8::try_from(self.0)
            .map(char::from)
            .map_err(|_| TinyError::InvalidAscii(self.to_string()))
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
        Byte(
            (self.opcode.0 as i32)
                .saturating_mul(1_000)
                .saturating_add(self.operand.0 as i32),
        )
    }
}
