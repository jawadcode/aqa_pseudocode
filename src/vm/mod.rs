use std::fmt::{Debug, Display};

use self::value::{Type, VMResult, Value};

pub mod value;

pub type RegNum = usize;
pub type ConstIdx = usize;
pub type Addr = usize;

pub struct VM {
    ip: usize,
    regs: Vec<Value>,
    stack: Vec<Value>,
    chunk: Chunk,
}

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum Op {
    Push(ConstIdx),
    Pop,
    DefineGlobal(ConstIdx),

    Add,
    Sub,
    Mul,
    Div,

    Less,
    LessEq,
    Greater,
    GreaterEq,
    Eq,
    NotEq,

    Not,
    Neg,

    Jump(Addr),
    JumpTrue(Addr),
    JumpFalse(Addr),

    Print,
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub consts: Vec<Value>,
    pub ops: Vec<Op>,
}

impl Default for Chunk {
    fn default() -> Self {
        Chunk {
            consts: vec![],
            ops: vec![],
        }
    }
}

#[derive(Debug, Clone)]
pub enum VMError {
    CannotPerform {
        op: &'static str,
        lhs: Type,
        rhs: Type,
    },
    IncorrectType {
        expected: Type,
        got: Type,
    },
}

impl Display for VMError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VMError::CannotPerform { op, lhs, rhs } => {
                write!(f, "Type Error: Could not perform '{op}' on {lhs} and {rhs}")
            }
            VMError::IncorrectType { expected, got } => {
                write!(f, "Type Error: Expected {expected}, got {got}")
            }
        }
    }
}

macro_rules! binop {
    ($self:ident, $op:tt) => {{
        let rhs = $self.pop();
        let lhs = $self.pop();
        $self.stack.push((lhs $op rhs)?);
    }};
}

macro_rules! binop_cmp {
    ($self:ident, $op:tt) => {{
        let rhs = $self.pop();
        let lhs = $self.pop();
        $self.stack.push(Value::Bool(lhs $op rhs));
    }};
}

macro_rules! unop {
    ($self:ident, $op:tt) => {{
        let value = $self.pop();
        $self.stack.push(($op value)?)
    }};
}

impl Default for VM {
    fn default() -> Self {
        Self {
            ip: 0,
            regs: vec![Value::Null; 256],
            stack: vec![],
            chunk: Default::default(),
        }
    }
}

impl VM {
    pub fn run(&mut self) -> VMResult<()> {
        while self.ip < self.chunk.ops.len() {
            match self.chunk.ops[self.ip].clone() {
                Op::Push(c) => self.stack.push(self.chunk.consts[c].clone()),
                Op::Pop => {
                    self.pop();
                }
                Op::DefineGlobal(c) => todo!(),

                Op::Add => binop!(self, +),
                Op::Sub => binop!(self, -),
                Op::Mul => binop!(self, *),
                Op::Div => binop!(self, /),

                Op::Less => binop_cmp!(self, <),
                Op::LessEq => binop_cmp!(self, <=),
                Op::Greater => binop_cmp!(self, >),
                Op::GreaterEq => binop_cmp!(self, >=),
                Op::Eq => binop_cmp!(self, ==),
                Op::NotEq => binop_cmp!(self, !=),

                Op::Not => unop!(self, !),
                Op::Neg => unop!(self, -),

                Op::Jump(addr) => {
                    self.ip = addr;
                    continue;
                }
                Op::JumpTrue(addr) => {
                    let cond: VMResult<bool> = self.pop().into();
                    if cond? {
                        self.ip = addr;
                        continue;
                    }
                }
                Op::JumpFalse(addr) => {
                    let cond: VMResult<bool> = self.pop().into();
                    if !cond? {
                        self.ip = addr;
                        continue;
                    }
                }
                Op::Print => println!("{}", self.pop()),
            }
            self.ip += 1;
        }

        Ok(())
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Empty Stack")
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Push(_) => "PUSH",
                Op::Pop => "POP",
                Op::DefineGlobal(_) => "DEFINEGLOBAL",
                Op::Add => "+",
                Op::Sub => "-",
                Op::Mul => "*",
                Op::Div => "/",
                Op::Less => "<",
                Op::LessEq => "<=",
                Op::Greater => ">",
                Op::GreaterEq => ">=",
                Op::Eq => "==",
                Op::NotEq => "!=",
                Op::Not => "!",
                Op::Neg => "-",
                Op::Jump(_) => "JUMP",
                Op::JumpTrue(_) => "JUMPTRUE",
                Op::JumpFalse(_) => "JUMPFALSE",
                Op::Print => "PRINT",
            }
        )
    }
}
