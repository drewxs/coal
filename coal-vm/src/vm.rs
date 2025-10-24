use std::{collections::HashMap, rc::Rc};

use coal_compiler::{Bytecode, Opcode, read_u16};
use coal_objects::{Closure, CompiledFunc, Constant, FALSE, Object, TRUE};

use crate::Frame;

pub const STACK_SIZE: usize = 2048;
pub const GLOBALS_SIZE: usize = 65536;
pub const MAX_FRAMES: usize = 1024;

#[derive(Clone, Debug, Default)]
pub struct VM {
    pub globals: Vec<Rc<Object>>,
    pub constants: Vec<Rc<Constant>>,
    pub stack: Vec<Rc<Object>>,
    pub sp: usize,
    pub frames: Vec<Frame>,
    pub frame_idx: usize,
}

impl VM {
    pub fn new() -> Self {
        VM {
            globals: (0..GLOBALS_SIZE).map(|_| Rc::new(Object::Nil)).collect(),
            constants: vec![],
            stack: (0..STACK_SIZE).map(|_| Rc::new(Object::Nil)).collect(),
            sp: 0,
            frames: vec![Frame::default(); MAX_FRAMES],
            frame_idx: 1,
        }
    }

    pub fn run(&mut self) {
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
                    let i = read_u16(&ins[ip + 1..ip + 3]) as usize;
                    self.curr_frame().ip += 2;
                    self.push(Rc::new((*self.constants[i]).clone().into()));
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
                    let rhs = self.popv();
                    let lhs = self.popv();
                    self.push(Rc::new(lhs + rhs));
                }
                Opcode::Sub => {
                    let rhs = self.popv();
                    let lhs = self.popv();
                    self.push(Rc::new(lhs - rhs));
                }
                Opcode::Mul => {
                    let rhs = self.popv();
                    let lhs = self.popv();
                    self.push(Rc::new(lhs * rhs));
                }
                Opcode::Div => {
                    let rhs = self.popv();
                    let lhs = self.popv();
                    self.push(Rc::new(lhs / rhs));
                }
                Opcode::Rem => {
                    let rhs = self.popv();
                    let lhs = self.popv();
                    self.push(Rc::new(lhs % rhs));
                }
                Opcode::EQ => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Rc::new(Object::from(lhs == rhs)));
                }
                Opcode::NEQ => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Rc::new(Object::from(lhs != rhs)));
                }
                Opcode::LT => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Rc::new(Object::from(lhs < rhs)));
                }
                Opcode::LTE => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Rc::new(Object::from(lhs <= rhs)));
                }
                Opcode::GT => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Rc::new(Object::from(lhs > rhs)));
                }
                Opcode::GTE => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Rc::new(Object::from(lhs >= rhs)));
                }
                Opcode::Minus => {
                    let result = match self.popv() {
                        Object::I32(i) => Object::I32(-i),
                        Object::I64(i) => Object::I64(-i),
                        Object::I128(i) => Object::I128(-i),
                        Object::F32(f) => Object::F32(-f),
                        Object::F64(f) => Object::F64(-f),
                        _ => unreachable!(),
                    };
                    self.push(Rc::new(result));
                }
                Opcode::Bang => match *self.pop() {
                    Object::Nil | FALSE => self.push(Rc::new(TRUE)),
                    _ => self.push(Rc::new(FALSE)),
                },
                Opcode::Jump => {
                    let pos = read_u16(&ins[ip + 1..ip + 3]) as i32;
                    self.curr_frame().ip = pos.saturating_sub(1);
                }
                Opcode::JumpFalse => {
                    let pos = read_u16(&ins[ip + 1..ip + 3]) as i32;
                    self.curr_frame().ip += 2;

                    let cond = self.pop();
                    if !cond.is_truthy() {
                        self.curr_frame().ip = pos.saturating_sub(1);
                    }
                }
                Opcode::GetGlobal => {
                    let idx = read_u16(&ins[ip + 1..ip + 3]) as usize;
                    self.curr_frame().ip += 2;
                    self.push(Rc::clone(&self.globals[idx]));
                }
                Opcode::SetGlobal => {
                    let idx = read_u16(&ins[ip + 1..ip + 3]) as usize;
                    self.curr_frame().ip += 2;
                    self.globals[idx] = self.pop();
                }
                Opcode::List => {
                    let count = read_u16(&ins[ip + 1..ip + 3]) as usize;
                    self.curr_frame().ip += 2;

                    let mut data = Vec::with_capacity(count);
                    for i in self.sp - count..self.sp {
                        data.push(Rc::clone(&self.stack[i]));
                    }

                    self.sp -= count;
                    self.push(Rc::new(Object::List(data)));
                }
                Opcode::Hash => {
                    let count = read_u16(&ins[ip + 1..ip + 3]) as usize;
                    self.curr_frame().ip += 2;

                    let mut data = HashMap::new();
                    for i in (self.sp - count..self.sp).step_by(2) {
                        let k = Rc::clone(&self.stack[i]);
                        let v = Rc::clone(&self.stack[i + 1]);
                        data.insert(k, v);
                    }

                    self.sp -= count;
                    self.push(Rc::new(Object::Map(data)));
                }
                Opcode::Index => {
                    let idx = self.popv();
                    let lhs = self.popv();

                    match (&lhs, &idx) {
                        (Object::Str(s), Object::I32(i)) => {
                            let len = s.len() as i32;
                            if len == 0 || *i > 0 && *i >= len || *i < 0 && *i < -len {
                                self.push(Rc::new(Object::Nil));
                            } else {
                                let i = if *i < 0 { len + *i } else { *i };
                                let s = s.chars().nth(i as usize).unwrap().to_string();
                                self.push(Rc::new(Object::Str(s)));
                            }
                        }
                        (Object::List(l), Object::I32(i)) => {
                            let len = l.len() as i32;
                            if len == 0 || *i > 0 && *i >= len || *i < 0 && *i < -len {
                                self.push(Rc::new(Object::Nil));
                            } else {
                                let i = if *i < 0 { len + *i } else { *i };
                                self.push(Rc::clone(&l[i as usize]));
                            }
                        }
                        (Object::Map(m), k) if idx.is_hashable() => match m.get(k) {
                            Some(v) => self.push(Rc::clone(v)),
                            None => self.push(Rc::new(Object::Nil)),
                        },
                        _ => {}
                    }
                }
                Opcode::Call => {
                    let n_args = ins[ip + 1] as usize;
                    self.curr_frame().ip += 1;

                    let callee = &*self.stack[self.sp.saturating_sub(1).saturating_sub(n_args)];
                    match callee {
                        Object::Closure(cl) => {
                            let frame = Frame::new(cl.clone(), self.sp - n_args);
                            self.sp = frame.base_ptr + cl.func.n_locals;
                            self.push_frame(frame);
                        }
                        Object::Builtin(_) => {}
                        _ => {}
                    }
                }
                Opcode::RetVal => {
                    let val = self.pop();
                    let frame = self.pop_frame();
                    self.sp = frame.base_ptr.saturating_sub(1);
                    self.push(val);
                }
                Opcode::Ret => {
                    let frame = self.pop_frame();
                    self.sp = frame.base_ptr.saturating_sub(1);
                    self.push(Rc::new(Object::Nil));
                }
                Opcode::GetLocal => {
                    let idx = ins[ip + 1] as usize;
                    self.curr_frame().ip += 1;
                    let base = self.curr_frame().base_ptr;
                    self.push(Rc::clone(&self.stack[base + idx]));
                }
                Opcode::SetLocal => {
                    let idx = ins[ip + 1] as usize;
                    self.curr_frame().ip += 1;
                    let base = self.curr_frame().base_ptr;
                    self.stack[base + idx] = self.pop();
                }
                Opcode::Closure => {
                    let idx = read_u16(&ins[ip + 1..ip + 3]) as usize;
                    let n_free = read_u16(&ins[ip + 3..ip + 5]) as usize;

                    self.curr_frame().ip += 3;

                    match self.constants[idx].as_ref() {
                        Constant::Func(f) => {
                            let mut free: Vec<Rc<Object>> = Vec::with_capacity(n_free);
                            for var in &mut free {
                                *var = self.stack[self.sp - n_free + 1].clone();
                            }
                            self.sp = self.sp.saturating_sub(n_free);
                            let cl = Closure {
                                func: Rc::new(f.clone()),
                                free,
                            };
                            self.push(Rc::new(Object::Closure(cl)));
                        }
                        _ => unreachable!(),
                    }
                }
                Opcode::GetFree => {
                    let idx = ins[ip + 1] as usize;
                    self.curr_frame().ip += 1;
                    let cl = self.curr_frame().cl.clone();
                    self.push(Rc::clone(&cl.free[idx]));
                }
                Opcode::CurrClosure => {
                    let cl = self.curr_frame().cl.clone();
                    self.push(Rc::new(Object::Closure(cl)));
                }
                _ => {}
            }
        }
    }

    pub fn push(&mut self, o: Rc<Object>) {
        if self.sp >= STACK_SIZE {
            panic!("stack overflow");
        }
        self.stack[self.sp] = o;
        self.sp += 1;
    }

    pub fn pop(&mut self) -> Rc<Object> {
        let o = Rc::clone(&self.stack[self.sp.saturating_sub(1)]);
        self.sp = self.sp.saturating_sub(1);
        o
    }

    pub fn popv(&mut self) -> Object {
        let o = Rc::clone(&self.stack[self.sp.saturating_sub(1)]);
        self.sp = self.sp.saturating_sub(1);
        (*o).clone()
    }

    pub fn curr_frame(&mut self) -> &mut Frame {
        &mut self.frames[self.frame_idx - 1]
    }

    pub fn push_frame(&mut self, frame: Frame) {
        self.frames[self.frame_idx] = frame;
        self.frame_idx += 1;
    }

    pub fn pop_frame(&mut self) -> Frame {
        self.frame_idx -= 1;
        self.frames[self.frame_idx].clone()
    }

    pub fn last_stack_obj(&mut self) -> Rc<Object> {
        self.stack[self.sp].clone()
    }

    pub fn print_stack(&self) {
        let stack = self
            .stack
            .iter()
            .enumerate()
            .filter(|(_, o)| ***o != Object::Nil)
            .map(|(i, o)| (i, o.to_string()))
            .collect::<Vec<_>>();
        println!("stack: {stack:#?}");

        let globals = self
            .globals
            .iter()
            .enumerate()
            .filter(|(_, o)| ***o != Object::Nil)
            .map(|(i, o)| (i, o.to_string()))
            .collect::<Vec<_>>();
        println!("globals: {globals:#?}");
    }
}

impl From<Bytecode> for VM {
    fn from(bytecode: Bytecode) -> Self {
        let mut vm = VM::new();
        vm.constants = bytecode.constants;
        vm.frames[0] = Frame::new(
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
        vm
    }
}

impl From<Vec<u8>> for VM {
    fn from(bytes: Vec<u8>) -> Self {
        Self::from(Bytecode::from(bytes))
    }
}
