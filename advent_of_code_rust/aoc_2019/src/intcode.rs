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
            99 => Self::Halt,
            _ => panic!("{num} is not a proper operation type"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Mode {
    Position,
    Immediate,
}

impl Mode {
    fn from_isize(num: isize) -> Self {
        match num {
            0 => Self::Position,
            1 => Self::Immediate,
            _ => panic!("{num} is not a proper mode"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IntCode {
    program_counter: usize,
    buffer: Vec<isize>,
    input_buffer: Vec<isize>,
    last_output: isize,
}

#[derive(Debug, PartialEq, Eq)]
enum Operation {
    BinOp(BinOp, [Mode; 2], [isize; 2], usize),
    CmpOp(CmpOp, [Mode; 2], [isize; 2]),
    Halt,
    Input(usize),
    Output(usize, Mode),
}

impl IntCode {
    fn set_mode_and_operands(
        &self,
        mut imodes: isize,
        nb_operands: usize,
        modes: &mut [Mode],
        operands: &mut [isize],
    ) {
        for i in 0..nb_operands {
            let mode = Mode::from_isize(imodes % 10);
            modes[i] = mode;
            let operand = self.buffer[self.program_counter + i + 1];
            operands[i] = operand;
            imodes /= 10;
        }
    }

    fn create_binop(&self, binop: BinOp, imodes: isize) -> Operation {
        let mut modes = [Mode::Position; 2];
        let mut operands = [0; 2];
        self.set_mode_and_operands(imodes, 2, &mut modes, &mut operands);
        Operation::BinOp(
            binop,
            modes,
            operands,
            self.buffer[self.program_counter + 3] as usize,
        )
    }

    fn create_cmpop(&self, cmpop: CmpOp, imodes: isize) -> Operation {
        let mut modes = [Mode::Position; 2];
        let mut operands = [0; 2];
        self.set_mode_and_operands(imodes, 2, &mut modes, &mut operands);
        Operation::CmpOp(cmpop, modes, operands)
    }

    fn create_halt(&self) -> Operation {
        Operation::Halt
    }

    fn create_input(&self) -> Operation {
        Operation::Input(self.buffer[self.program_counter + 1] as usize)
    }

    fn create_output(&self, mode: isize) -> Operation {
        let mode = Mode::from_isize(mode % 10);
        Operation::Output(self.buffer[self.program_counter + 1] as usize, mode)
    }

    fn operation(&self) -> Operation {
        let instruction = self.buffer[self.program_counter];
        let operation = OpType::from_isize(instruction % 100);
        let modes = instruction / 100;
        match operation {
            OpType::BinOp(binop) => self.create_binop(binop, modes),
            OpType::CmpOp(cmpop) => self.create_cmpop(cmpop, modes),
            OpType::Input => self.create_input(),
            OpType::Output => self.create_output(modes),
            _ => self.create_halt(),
        }
    }

    pub fn intcode_loop(&mut self) -> isize {
        'halt: loop {
            // println!("operation: {:?}", self.operation());
            match self.operation() {
                Operation::BinOp(binop, modes, operands, result) => {
                    let operands: Vec<isize> = operands
                        .iter()
                        .enumerate()
                        .map(|(index, value)| match modes[index] {
                            Mode::Position => self.buffer[*value as usize],
                            Mode::Immediate => *value,
                        })
                        .collect();
                    let binop = binop.to_fn();
                    self.buffer[result] = binop(operands[0], operands[1]);
                    self.program_counter += 4;
                }

                Operation::CmpOp(cmpop, modes, operands) => {
                    let operands: Vec<isize> = operands
                        .iter()
                        .enumerate()
                        .map(|(index, value)| match modes[index] {
                            Mode::Position => self.buffer[*value as usize],
                            Mode::Immediate => *value,
                        })
                        .collect();
                    // println!("operands: {:?}", operands);
                    let cmpop = cmpop.to_fn();
                    if cmpop(operands[0]) {
                        self.program_counter = operands[1] as usize
                    } else {
                        self.program_counter += 3
                    }
                }

                Operation::Halt => break 'halt,
                Operation::Input(index) => {
                    self.buffer[index] = self.input_buffer.pop().unwrap();
                    self.program_counter += 2;
                }
                Operation::Output(index, mode) => {
                    let output = match mode {
                        Mode::Immediate => index as isize,
                        Mode::Position => self.buffer[index],
                    };
                    self.last_output = output;
                    println!("{output}");
                    self.program_counter += 2;
                }
            };
            // println!("{:?}", self);
        }
        self.buffer[0]
    }

    pub fn set(&mut self, index: usize, value: isize) {
        self.buffer[index] = value
    }

    pub fn set_input(&mut self, value: isize) {
        self.input_buffer.push(value);
    }

    pub fn new(string: &str) -> Self {
        let buffer = string
            .trim()
            .split(',')
            .map(|v| v.parse::<isize>().unwrap())
            .collect();
        Self {
            program_counter: 0,
            buffer,
            input_buffer: vec![0; 0],
            last_output: 0,
        }
    }
}
