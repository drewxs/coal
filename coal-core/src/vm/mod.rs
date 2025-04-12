use std::rc::Rc;

use crate::{read_u16, Object};

mod frame;

use frame::Frame;

use super::{Bytecode, Closure, CompileError, CompiledFunc, Opcode, FALSE, TRUE};

pub const STACK_SIZE: usize = 2048;
pub const GLOBALS_SIZE: usize = 65536;
pub const MAX_FRAMES: usize = 1024;

#[derive(Clone, Debug)]
pub struct VM {
    pub globals: Vec<Rc<Object>>,
    pub constants: Vec<Rc<Object>>,
    pub stack: Vec<Rc<Object>>,
    pub sp: usize,
    pub frames: Vec<Frame>,
    pub frame_idx: usize,
}

impl VM {
    pub fn new(bytecode: Bytecode, globals: Vec<Rc<Object>>) -> Self {
        let mut vm = VM::from(bytecode);
        vm.globals = globals;
        vm
    }

    pub fn push(&mut self, o: Rc<Object>) {
        if self.sp >= STACK_SIZE {
            panic!("stack overflow");
        }
        self.stack[self.sp] = o;
        self.sp += 1;
    }

    pub fn pop(&mut self) -> Object {
        let o = (*self.stack[self.sp - 1]).clone();
        self.sp -= 1;
        o
    }

    pub fn curr_frame(&mut self) -> &mut Frame {
        &mut self.frames[self.frame_idx - 1]
    }

    pub fn push_frame(&mut self, frame: Frame) {
        self.frames.push(frame);
        self.frame_idx += 1;
    }

    pub fn pop_frame(&mut self) -> Frame {
        self.frame_idx -= 1;
        self.frames[self.frame_idx].clone()
    }

    pub fn last_stack_obj(&mut self) -> Rc<Object> {
        self.stack[self.sp].clone()
    }

    pub fn run(&mut self) -> Result<(), CompileError> {
        while self.curr_frame().ip < self.curr_frame().instructions().len() as i32 - 1 {
            self.curr_frame().ip += 1;

            let ip = self.curr_frame().ip as usize;
            let ins = self.curr_frame().instructions().0;

            let op = ins[ip];
            let opcode = Opcode::from(op);

            match opcode {
                Opcode::Nil => {
                    self.push(Rc::new(Object::Nil));
                }
                Opcode::Const => {
                    let const_idx = read_u16(&ins[ip + 1..ip + 3]) as usize;
                    self.curr_frame().ip += 2;
                    self.push(Rc::clone(&self.constants[const_idx]));
                }
                Opcode::Pop => {
                    self.pop();
                }
                Opcode::True => {
                    self.push(Rc::new(TRUE));
                }
                Opcode::False => {
                    self.push(Rc::new(FALSE));
                }
                Opcode::Add => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Rc::new(lhs + rhs));
                }
                Opcode::Sub => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Rc::new(lhs - rhs));
                }
                Opcode::Mul => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Rc::new(lhs * rhs));
                }
                Opcode::Div => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Rc::new(lhs / rhs));
                }
                Opcode::Rem => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Rc::new(lhs % rhs));
                }
                Opcode::EQ => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Rc::new(Object::Bool(lhs == rhs)));
                }
                Opcode::NEQ => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Rc::new(Object::Bool(lhs != rhs)));
                }
                Opcode::LT => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Rc::new(Object::Bool(lhs < rhs)));
                }
                Opcode::LTE => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Rc::new(Object::Bool(lhs <= rhs)));
                }
                Opcode::GT => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Rc::new(Object::Bool(lhs > rhs)));
                }
                Opcode::GTE => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Rc::new(Object::Bool(lhs >= rhs)));
                }
                Opcode::Minus => {
                    let result = match self.pop() {
                        Object::I32(i) => Object::I32(-i),
                        Object::I64(i) => Object::I64(-i),
                        Object::I128(i) => Object::I128(-i),
                        Object::F32(f) => Object::F32(-f),
                        Object::F64(f) => Object::F64(-f),
                        _ => unreachable!(),
                    };
                    self.push(Rc::new(result));
                }
                Opcode::Bang => match self.pop() {
                    Object::Nil | FALSE => self.push(Rc::new(TRUE)),
                    _ => self.push(Rc::new(FALSE)),
                },
                Opcode::Jump => {
                    let pos = read_u16(&ins[ip + 1..ip + 3]) as i32;
                    self.curr_frame().ip = pos.saturating_sub(1);
                }
                Opcode::JumpIfNot => {
                    let pos = read_u16(&ins[ip + 1..ip + 3]) as i32;
                    self.curr_frame().ip += 2;
                    let cond = self.pop();
                    if cond.is_truthy() {
                        self.curr_frame().ip = pos.saturating_sub(1);
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }
}

impl From<Bytecode> for VM {
    fn from(bytecode: Bytecode) -> Self {
        let mut frames = vec![Frame::default(); MAX_FRAMES];
        frames[0] = Frame::new(
            Closure {
                func: Rc::new(CompiledFunc {
                    instructions: bytecode.instructions.0,
                    n_locals: 0,
                    n_params: 0,
                }),
                free: vec![],
            },
            0,
        );

        VM {
            globals: (0..GLOBALS_SIZE).map(|_| Rc::new(Object::Nil)).collect(),
            constants: bytecode.constants,
            stack: (0..STACK_SIZE).map(|_| Rc::new(Object::Nil)).collect(),
            sp: 0,
            frames,
            frame_idx: 1,
        }
    }
}
