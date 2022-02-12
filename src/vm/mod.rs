use std::fmt::{Debug, Display};

use self::value::{Type, VMResult, Value};

pub mod value;

pub type RegNum = usize;
pub type ConstIdx = usize;
pub type Addr = usize;

pub struct VM<'chunk> {
    ip: usize,
    regs: Vec<Value>,
    stack: Vec<Value>,
    chunk: &'chunk Chunk,
}

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum Op {
    Push(ConstIdx),
    Pop,
    Load(RegNum),
    Move(RegNum),
    MoveConst(RegNum, ConstIdx),

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

impl<'chunk> VM<'chunk> {
    pub fn new(chunk: &'chunk Chunk) -> Self {
        Self {
            ip: 0,
            regs: vec![Value::Null; 256],
            stack: vec![],
            chunk,
        }
    }

    pub fn run(&mut self) -> VMResult<()> {
        while self.ip < self.chunk.ops.len() {
            match self.chunk.ops[self.ip].clone() {
                Op::Push(c) => self.stack.push(self.chunk.consts[c].clone()),
                Op::Pop => {
                    self.pop();
                }
                Op::Load(r) => self.stack.push(self.regs[r].clone()),
                Op::Move(r) => self.regs[r] = self.pop(),
                Op::MoveConst(r, c) => self.regs[r] = self.chunk.consts[c].clone(),

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
        match self {
            Op::Push(_) => write!(f, "push"),
            Op::Pop => write!(f, "pop"),
            Op::Load(_) => write!(f, "pop"),
            Op::Move(_) => write!(f, "move"),
            Op::MoveConst(_, _) => write!(f, "store"),
            Op::Add => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::Less => write!(f, "<"),
            Op::LessEq => write!(f, "<="),
            Op::Greater => write!(f, ">"),
            Op::GreaterEq => write!(f, ">="),
            Op::Eq => write!(f, "=="),
            Op::NotEq => write!(f, "!="),
            Op::Jump(_) => write!(f, "jump"),
            Op::JumpTrue(_) => write!(f, "jumpif"),
            Op::JumpFalse(_) => write!(f, "jumpelse"),
            Op::Print => write!(f, "print"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{value::Value, Chunk, Op, VM};

    #[test]
    fn basic_ops() {
        let chunk = Chunk {
            consts: vec![
                Value::Number(0.0),
                Value::Number(1.0),
                Value::Number(10000.0),
            ],
            ops: vec![
                // Loop
                Op::MoveConst(0, 0),
                Op::Load(0),
                Op::Push(1),
                Op::Add,
                Op::Move(0),
                // Print
                Op::Load(0),
                Op::Load(0),
                Op::Mul,
                Op::Print,
                // Check
                Op::Load(0),
                Op::Push(1),
                Op::Add,
                Op::Push(2),
                Op::LessEq,
                Op::JumpTrue(1),
            ],
        };

        let mut vm = VM::new(&chunk);
        vm.run().unwrap_or_else(|err| eprintln!("{err}"));
    }
}
