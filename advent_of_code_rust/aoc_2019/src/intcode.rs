use std::collections::VecDeque;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum BinOp {
    Add,
    Mult,
    LessThan,
    Equal,
}

impl BinOp {
    fn to_fn(self) -> fn(isize, isize) -> isize {
        match self {
            Self::Add => std::ops::Add::add,
            Self::Mult => std::ops::Mul::mul,
            Self::Equal => |v1, v2| if v1 == v2 { 1 } else { 0 },
            Self::LessThan => |v1, v2| if v1 < v2 { 1 } else { 0 },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum CmpOp {
    IfTrue,
    IfFalse,
}
impl CmpOp {
    fn to_fn(self) -> fn(isize) -> bool {
        match self {
            Self::IfTrue => |v| v != 0,
            Self::IfFalse => |v| v == 0,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum OpType {
    BinOp(BinOp),
    CmpOp(CmpOp),
    Input,
    Output,
    Halt,
    Relative,
}

impl OpType {
    fn from_isize(num: isize) -> Self {
        match num {
            1 => Self::BinOp(BinOp::Add),
            2 => Self::BinOp(BinOp::Mult),
            3 => Self::Input,
            4 => Self::Output,
            5 => Self::CmpOp(CmpOp::IfTrue),
            6 => Self::CmpOp(CmpOp::IfFalse),
            7 => Self::BinOp(BinOp::LessThan),
            8 => Self::BinOp(BinOp::Equal),
            9 => Self::Relative,
            99 => Self::Halt,
            _ => panic!("{num} is not a proper operation type"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Mode {
    Position,
    Immediate,
    Relative,
}

impl Mode {
    fn from_isize(num: isize) -> Self {
        match num {
            0 => Self::Position,
            1 => Self::Immediate,
            2 => Self::Relative,
            _ => panic!("{num} is not a proper mode"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum State {
    Halted,
    WaitingInput,
    Running,
    Output(isize),
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct DynVec<T> {
    content: Vec<T>,
}

impl<T> DynVec<T>
where
    T: Clone + Default + Copy + std::fmt::Debug,
{
    pub fn of_vec(content: Vec<T>) -> Self {
        DynVec { content }
    }

    // pub fn new(length: usize, def_value: T) -> Self {
    //     DynVec {
    //         content: vec![def_value; length],
    //     }
    // }

    // fn new_empty() -> Self {
    //     DynVec {
    //         content: Vec::new(),
    //     }
    // }

    fn resize(&mut self, new_len: usize, value: T) {
        self.content.resize(new_len, value)
    }

    fn get(&mut self, index: usize) -> T {
        if index >= self.content.len() {
            self.resize(index * 2, T::default())
        };
        self.content[index]
    }

    fn set(&mut self, index: usize, value: T) {
        if index >= self.content.len() {
            self.resize(index * 2, T::default())
        };
        self.content[index] = value
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IntCode {
    identifier: usize,
    program_counter: usize,
    buffer: DynVec<isize>,
    input_buffer: VecDeque<isize>,
    last_output: isize,
    relative_base: isize,
    state: State,
}

#[derive(Debug, PartialEq, Eq)]
enum Operation {
    BinOp(BinOp, [Mode; 3], [isize; 3]),
    CmpOp(CmpOp, [Mode; 2], [isize; 2]),
    Halt,
    Input(usize, Mode),
    Output(isize, Mode),
    Relative(isize, Mode),
}

impl IntCode {
    fn set_mode_and_operands(
        &mut self,
        mut imodes: isize,
        nb_operands: usize,
        modes: &mut [Mode],
        operands: &mut [isize],
    ) {
        for i in 0..nb_operands {
            let mode = Mode::from_isize(imodes % 10);
            modes[i] = mode;
            let operand = self.buffer.get(self.program_counter + i + 1);
            operands[i] = operand;
            imodes /= 10;
        }
    }

    fn create_binop(&mut self, binop: BinOp, imodes: isize) -> Operation {
        let mut modes = [Mode::Position; 3];
        let mut operands = [0; 3];
        self.set_mode_and_operands(imodes, 3, &mut modes, &mut operands);
        Operation::BinOp(binop, modes, operands)
    }

    fn create_cmpop(&mut self, cmpop: CmpOp, imodes: isize) -> Operation {
        let mut modes = [Mode::Position; 2];
        let mut operands = [0; 2];
        self.set_mode_and_operands(imodes, 2, &mut modes, &mut operands);
        Operation::CmpOp(cmpop, modes, operands)
    }

    fn create_halt(&self) -> Operation {
        Operation::Halt
    }

    fn create_input(&mut self, mode: isize) -> Operation {
        let mode = Mode::from_isize(mode % 10);
        Operation::Input(self.buffer.get(self.program_counter + 1) as usize, mode)
    }

    fn create_output(&mut self, mode: isize) -> Operation {
        let mode = Mode::from_isize(mode % 10);
        Operation::Output(self.buffer.get(self.program_counter + 1), mode)
    }
    fn create_relative(&mut self, mode: isize) -> Operation {
        let mode = Mode::from_isize(mode % 10);
        Operation::Relative(self.buffer.get(self.program_counter + 1), mode)
    }

    fn operation(&mut self) -> Operation {
        let instruction = self.buffer.get(self.program_counter);
        let operation = OpType::from_isize(instruction % 100);
        let modes = instruction / 100;
        match operation {
            OpType::BinOp(binop) => self.create_binop(binop, modes),
            OpType::CmpOp(cmpop) => self.create_cmpop(cmpop, modes),
            OpType::Input => self.create_input(modes),
            OpType::Output => self.create_output(modes),
            OpType::Relative => self.create_relative(modes),
            OpType::Halt => self.create_halt(),
        }
    }

    pub fn intcode_loop(&mut self, stop_at_output: bool) -> State {
        'halt: loop {
            match self.operation() {
                Operation::BinOp(binop, modes, operands) => {
                    let binop = binop.to_fn();
                    let op1 = self.get(operands[0], modes[0]);
                    let op2 = self.get(operands[1], modes[1]);
                    self.set(operands[2], binop(op1, op2), modes[2]);
                    self.program_counter += 4;
                }

                Operation::CmpOp(cmpop, modes, operands) => {
                    let cmpop = cmpop.to_fn();
                    if cmpop(self.get(operands[0], modes[0])) {
                        self.program_counter = self.get(operands[1], modes[1]) as usize
                    } else {
                        self.program_counter += 3
                    }
                }

                Operation::Halt => {
                    self.state = State::Halted;
                    break 'halt;
                }

                Operation::Input(index, mode) => {
                    if let Some(input) = self.input_buffer.pop_front() {
                        self.set(index as isize, input, mode);
                        self.program_counter += 2;
                    } else {
                        self.state = State::WaitingInput;
                        break 'halt;
                    }
                }

                Operation::Output(index, mode) => {
                    let output = self.get(index, mode);
                    self.last_output = output;
                    self.program_counter += 2;
                    if stop_at_output {
                        self.state = State::Output(output);
                        break 'halt;
                    } else {
                        println!("{output}")
                    }
                }

                Operation::Relative(value, mode) => {
                    let value = self.get(value, mode);
                    self.relative_base += value;
                    self.program_counter += 2
                }
            };
            // println!("{:?}", self);
        }
        self.state
    }

    #[inline]
    pub fn set_input(&mut self, value: isize) {
        self.input_buffer.push_back(value);
    }

    #[inline]
    pub fn set_identifier(&mut self, identifier: usize) {
        self.identifier = identifier
    }

    #[inline]
    pub fn get_identifier(&self) -> usize {
        self.identifier
    }

    #[inline]
    pub fn get_output(&self) -> isize {
        self.last_output
    }

    #[inline]
    pub fn get(&mut self, index: isize, mode: Mode) -> isize {
        match mode {
            Mode::Immediate => index,
            Mode::Position => self.buffer.get(index as usize),
            Mode::Relative => self.buffer.get((index + self.relative_base) as usize),
        }
    }

    #[inline]
    pub fn set(&mut self, index: isize, value: isize, mode: Mode) {
        match mode {
            Mode::Immediate => panic!("Can't have immediate mode for setters"),
            Mode::Position => self.buffer.set(index as usize, value),
            Mode::Relative => self
                .buffer
                .set((index + self.relative_base) as usize, value),
        }
    }

    pub fn new(string: &str) -> Self {
        let buffer = DynVec::of_vec(
            string
                .trim()
                .split(',')
                .map(|v| v.parse::<isize>().unwrap())
                .collect(),
        );
        Self {
            identifier: 0,
            program_counter: 0,
            buffer,
            input_buffer: VecDeque::new(),
            last_output: 0,
            relative_base: 0,
            state: State::Running,
        }
    }
}
