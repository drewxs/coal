use std::{collections::HashMap, rc::Rc};

use coal_compiler::{Bytecode, Opcode, read_u16};
use coal_objects::{Builtin, Closure, CompiledFunc, Constant, FALSE, Object, TRUE, builtin_defs};

use crate::Frame;

pub const STACK_SIZE: usize = 2048;
pub const GLOBALS_SIZE: usize = 65536;
pub const MAX_FRAMES: usize = 1024;

#[derive(Clone, Debug)]
pub struct VM {
    pub globals: Vec<Object>,
    pub constants: Vec<Rc<Constant>>,
    pub const_objects: Vec<Object>,
    pub stack: Vec<Object>,
    pub sp: usize,
    pub frames: Vec<Frame>,
    pub frame_idx: usize,
    pub builtins: Vec<Builtin>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            globals: (0..GLOBALS_SIZE).map(|_| Object::Nil).collect(),
            constants: vec![],
            const_objects: vec![],
            stack: (0..STACK_SIZE).map(|_| Object::Nil).collect(),
            sp: 0,
            frames: vec![Frame::default(); MAX_FRAMES],
            frame_idx: 1,
            builtins: builtin_defs(),
        }
    }

    pub fn run(&mut self) {
        loop {
            let frame_idx = self.frame_idx - 1;
            let func = Rc::clone(&self.frames[frame_idx].cl.func);
            let ins: &[u8] = &func.instructions;

            if self.frames[frame_idx].ip >= ins.len() as i32 - 1 {
                break;
            }
            self.frames[frame_idx].ip += 1;
            let ip = self.frames[frame_idx].ip as usize;

            let op = ins[ip];
            let opcode = Opcode::from(op);

            match opcode {
                Opcode::Nil => {
                    self.push(Object::Nil);
                }
                Opcode::Const => {
                    let i = read_u16(&ins[ip + 1..ip + 3]) as usize;
                    self.curr_frame().ip += 2;
                    self.push(self.const_objects[i].clone());
                }
                Opcode::Pop => {
                    self.pop();
                }
                Opcode::True => {
                    self.push(TRUE);
                }
                Opcode::False => {
                    self.push(FALSE);
                }
                Opcode::Add => {
                    if let (Object::I32(l), Object::I32(r)) =
                        (&self.stack[self.sp - 2], &self.stack[self.sp - 1])
                    {
                        let res = Object::I32(l.wrapping_add(*r));
                        self.sp -= 1;
                        self.stack[self.sp - 1] = res;
                    } else {
                        let rhs = self.pop();
                        let lhs = self.pop();
                        self.push(lhs + rhs);
                    }
                }
                Opcode::Sub => {
                    if let (Object::I32(l), Object::I32(r)) =
                        (&self.stack[self.sp - 2], &self.stack[self.sp - 1])
                    {
                        let res = Object::I32(l.wrapping_sub(*r));
                        self.sp -= 1;
                        self.stack[self.sp - 1] = res;
                    } else {
                        let rhs = self.pop();
                        let lhs = self.pop();
                        self.push(lhs - rhs);
                    }
                }
                Opcode::Mul => {
                    if let (Object::I32(l), Object::I32(r)) =
                        (&self.stack[self.sp - 2], &self.stack[self.sp - 1])
                    {
                        let res = Object::I32(l.wrapping_mul(*r));
                        self.sp -= 1;
                        self.stack[self.sp - 1] = res;
                    } else {
                        let rhs = self.pop();
                        let lhs = self.pop();
                        self.push(lhs * rhs);
                    }
                }
                Opcode::Div => {
                    if let (Object::I32(l), Object::I32(r)) =
                        (&self.stack[self.sp - 2], &self.stack[self.sp - 1])
                    {
                        let res = Object::I32(l / r);
                        self.sp -= 1;
                        self.stack[self.sp - 1] = res;
                    } else {
                        let rhs = self.pop();
                        let lhs = self.pop();
                        self.push(lhs / rhs);
                    }
                }
                Opcode::Rem => {
                    if let (Object::I32(l), Object::I32(r)) =
                        (&self.stack[self.sp - 2], &self.stack[self.sp - 1])
                    {
                        let res = Object::I32(l % r);
                        self.sp -= 1;
                        self.stack[self.sp - 1] = res;
                    } else {
                        let rhs = self.pop();
                        let lhs = self.pop();
                        self.push(lhs % rhs);
                    }
                }
                Opcode::EQ => {
                    let b = match (&self.stack[self.sp - 2], &self.stack[self.sp - 1]) {
                        (Object::I32(l), Object::I32(r)) => *l == *r,
                        _ => self.stack[self.sp - 2] == self.stack[self.sp - 1],
                    };
                    self.sp -= 1;
                    self.stack[self.sp - 1] = Object::from(b);
                }
                Opcode::NEQ => {
                    let b = match (&self.stack[self.sp - 2], &self.stack[self.sp - 1]) {
                        (Object::I32(l), Object::I32(r)) => *l != *r,
                        _ => self.stack[self.sp - 2] != self.stack[self.sp - 1],
                    };
                    self.sp -= 1;
                    self.stack[self.sp - 1] = Object::from(b);
                }
                Opcode::LT => {
                    let b = match (&self.stack[self.sp - 2], &self.stack[self.sp - 1]) {
                        (Object::I32(l), Object::I32(r)) => *l < *r,
                        _ => self.stack[self.sp - 2] < self.stack[self.sp - 1],
                    };
                    self.sp -= 1;
                    self.stack[self.sp - 1] = Object::from(b);
                }
                Opcode::LTE => {
                    let b = match (&self.stack[self.sp - 2], &self.stack[self.sp - 1]) {
                        (Object::I32(l), Object::I32(r)) => *l <= *r,
                        _ => self.stack[self.sp - 2] <= self.stack[self.sp - 1],
                    };
                    self.sp -= 1;
                    self.stack[self.sp - 1] = Object::from(b);
                }
                Opcode::GT => {
                    let b = match (&self.stack[self.sp - 2], &self.stack[self.sp - 1]) {
                        (Object::I32(l), Object::I32(r)) => *l > *r,
                        _ => self.stack[self.sp - 2] > self.stack[self.sp - 1],
                    };
                    self.sp -= 1;
                    self.stack[self.sp - 1] = Object::from(b);
                }
                Opcode::GTE => {
                    let b = match (&self.stack[self.sp - 2], &self.stack[self.sp - 1]) {
                        (Object::I32(l), Object::I32(r)) => *l >= *r,
                        _ => self.stack[self.sp - 2] >= self.stack[self.sp - 1],
                    };
                    self.sp -= 1;
                    self.stack[self.sp - 1] = Object::from(b);
                }
                Opcode::Minus => {
                    if let Object::I32(i) = &self.stack[self.sp - 1] {
                        self.stack[self.sp - 1] = Object::I32(-*i);
                    } else {
                        let result = match self.pop() {
                            Object::I32(i) => Object::I32(-i),
                            Object::I64(i) => Object::I64(-i),
                            Object::I128(i) => Object::I128(-i),
                            Object::F32(f) => Object::F32(-f),
                            Object::F64(f) => Object::F64(-f),
                            _ => unreachable!(),
                        };
                        self.push(result);
                    }
                }
                Opcode::Bang => {
                    let res = match self.pop() {
                        Object::Nil | FALSE => TRUE,
                        _ => FALSE,
                    };
                    self.push(res);
                }
                Opcode::Jump => {
                    let pos = read_u16(&ins[ip + 1..ip + 3]) as i32;
                    self.curr_frame().ip = pos - 1;
                }
                Opcode::JumpFalse => {
                    let pos = read_u16(&ins[ip + 1..ip + 3]) as i32;
                    self.curr_frame().ip += 2;

                    let cond = self.pop();
                    if !cond.is_truthy() {
                        self.curr_frame().ip = pos - 1;
                    }
                }
                Opcode::GetGlobal => {
                    let idx = read_u16(&ins[ip + 1..ip + 3]) as usize;
                    self.curr_frame().ip += 2;
                    let val = self.globals[idx].clone();
                    self.push(val);
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
                        data.push(self.stack[i].clone());
                    }

                    self.sp -= count;
                    self.push(Object::List(Rc::new(data)));
                }
                Opcode::ListRepeat => {
                    let count: usize = self.pop().try_into().unwrap_or(0);
                    let val = self.pop();
                    let mut data = Vec::with_capacity(count);
                    for _ in 0..count {
                        data.push(val.clone());
                    }
                    self.push(Object::List(Rc::new(data)));
                }
                Opcode::Hash => {
                    let count = read_u16(&ins[ip + 1..ip + 3]) as usize;
                    self.curr_frame().ip += 2;

                    let mut data = HashMap::new();
                    for i in (self.sp - count..self.sp).step_by(2) {
                        let k = self.stack[i].clone();
                        let v = self.stack[i + 1].clone();
                        data.insert(k, v);
                    }

                    self.sp -= count;
                    self.push(Object::Map(Rc::new(data)));
                }
                Opcode::Index => {
                    let idx = self.pop();
                    let lhs = self.pop();

                    match (&lhs, &idx) {
                        (Object::Str(s), Object::I32(i)) => {
                            let len = s.len() as i32;
                            if len == 0 || *i > 0 && *i >= len || *i < 0 && *i < -len {
                                self.push(Object::Nil);
                            } else {
                                let i = if *i < 0 { len + *i } else { *i };
                                let ch = s.chars().nth(i as usize).unwrap().to_string();
                                self.push(Object::Str(Rc::new(ch)));
                            }
                        }
                        (Object::List(l), Object::I32(i)) => {
                            let len = l.len() as i32;
                            if len == 0 || *i > 0 && *i >= len || *i < 0 && *i < -len {
                                self.push(Object::Nil);
                            } else {
                                let i = if *i < 0 { len + *i } else { *i };
                                self.push(l[i as usize].clone());
                            }
                        }
                        (Object::Map(m), k) if idx.is_hashable() => match m.get(k) {
                            Some(v) => self.push(v.clone()),
                            None => self.push(Object::Nil),
                        },
                        _ => {}
                    }
                }
                Opcode::SetIndex => {
                    let val = self.pop();
                    let idx = self.pop();
                    let mut coll = self.pop();

                    match (&mut coll, &idx) {
                        (Object::List(data), Object::I32(i)) => {
                            let len = data.len() as i32;
                            if !(len == 0 || *i >= len || *i < -len) {
                                let i = if *i < 0 { len + *i } else { *i };
                                Rc::make_mut(data)[i as usize] = val;
                            }
                        }
                        (Object::Map(data), k) if k.is_hashable() => {
                            Rc::make_mut(data).insert(idx, val);
                        }
                        _ => {}
                    }

                    self.push(coll);
                }
                Opcode::Call => {
                    let n_args = ins[ip + 1] as usize;
                    self.curr_frame().ip += 1;

                    // Clone the callee out so we don't hold a borrow on self.stack.
                    let callee = self.stack[self.sp - 1 - n_args].clone();
                    match callee {
                        Object::Closure(cl) => {
                            let base_ptr = self.sp - 1 - n_args;
                            let new_sp = base_ptr + 1 + cl.func.n_params + cl.func.n_locals;
                            let frame = Frame::new(cl, base_ptr);
                            self.sp = new_sp;
                            self.push_frame(frame);
                        }
                        Object::Builtin(b) => {
                            let args = &self.stack[self.sp - n_args..self.sp];
                            let res = (b.func)(args);
                            self.sp -= n_args + 1;
                            self.push(res.unwrap_or(Object::Nil));
                        }
                        _ => {}
                    }
                }
                Opcode::Method => {
                    let name_idx = read_u16(&ins[ip + 1..ip + 3]) as usize;
                    let n_args = ins[ip + 3] as usize;
                    self.curr_frame().ip += 3;

                    let name = match self.constants[name_idx].as_ref() {
                        Constant::Str(s) => s.clone(),
                        _ => unreachable!(),
                    };

                    let mut args = Vec::with_capacity(n_args);
                    for _ in 0..n_args {
                        args.push(self.pop());
                    }
                    args.reverse();

                    let mut receiver = self.pop();
                    let result = receiver.call(&name, &args);

                    self.push(result.unwrap_or(Object::Nil));
                    self.push(receiver);
                }
                Opcode::RetVal => {
                    let val = self.pop();
                    let frame = self.pop_frame();
                    self.sp = frame.base_ptr;
                    self.push(val);
                }
                Opcode::Ret => {
                    let frame = self.pop_frame();
                    self.sp = frame.base_ptr;
                    self.push(Object::Nil);
                }
                Opcode::SetLocal => {
                    let idx = ins[ip + 1] as usize;
                    self.curr_frame().ip += 1;
                    let base = self.curr_frame().base_ptr;
                    self.stack[base + 1 + idx] = self.pop();
                }
                Opcode::GetLocal => {
                    let idx = ins[ip + 1] as usize;
                    self.curr_frame().ip += 1;
                    let base = self.curr_frame().base_ptr;
                    self.push(self.stack[base + 1 + idx].clone());
                }
                Opcode::GetBuiltin => {
                    let idx = ins[ip + 1] as usize;
                    self.curr_frame().ip += 1;
                    let b = self.builtins[idx].clone();
                    self.push(Object::Builtin(b));
                }
                Opcode::Closure => {
                    let idx = read_u16(&ins[ip + 1..ip + 3]) as usize;
                    let n_free = ins[ip + 3] as usize;

                    self.curr_frame().ip += 3;

                    match self.constants[idx].as_ref() {
                        Constant::Func(f) => {
                            let mut free: Vec<Object> = Vec::with_capacity(n_free);
                            for i in 0..n_free {
                                free.push(self.stack[self.sp - n_free + i].clone());
                            }
                            self.sp -= n_free;
                            self.push(Object::Closure(Rc::new(Closure {
                                func: Rc::new(f.clone()),
                                free,
                            })));
                        }
                        _ => unreachable!(),
                    }
                }
                Opcode::GetFree => {
                    let idx = ins[ip + 1] as usize;
                    self.curr_frame().ip += 1;
                    let val = self.curr_frame().cl.free[idx].clone();
                    self.push(val);
                }
                Opcode::CurrClosure => {
                    let cl = Rc::clone(&self.curr_frame().cl);
                    self.push(Object::Closure(cl));
                }
            }
        }
    }

    #[inline(always)]
    pub fn push(&mut self, o: Object) {
        if self.sp >= STACK_SIZE {
            panic!("stack overflow");
        }
        self.stack[self.sp] = o;
        self.sp += 1;
    }

    #[inline(always)]
    pub fn pop(&mut self) -> Object {
        self.sp = self.sp.saturating_sub(1);
        self.stack[self.sp].clone()
    }

    #[inline(always)]
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

    pub fn last_stack_obj(&self) -> Object {
        self.stack[self.sp].clone()
    }

    pub fn print_stack(&self) {
        let stack = self
            .stack
            .iter()
            .enumerate()
            .filter(|(_, o)| **o != Object::Nil)
            .map(|(i, o)| (i, o.to_string()))
            .collect::<Vec<_>>();
        println!("stack: {stack:#?}");

        let globals = self
            .globals
            .iter()
            .enumerate()
            .filter(|(_, o)| **o != Object::Nil)
            .map(|(i, o)| (i, o.to_string()))
            .collect::<Vec<_>>();
        println!("globals: {globals:#?}");
    }
}

impl From<Bytecode> for VM {
    fn from(bytecode: Bytecode) -> Self {
        let mut vm = VM::new();
        vm.const_objects = bytecode
            .constants
            .iter()
            .map(|c| (**c).clone().into())
            .collect();
        vm.constants = bytecode.constants;
        vm.frames[0] = Frame::new(
            Rc::new(Closure {
                func: Rc::new(CompiledFunc {
                    instructions: bytecode.instructions.0,
                    n_locals: 0,
                    n_params: 0,
                }),
                free: vec![],
            }),
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
