use crate::parser::*;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub struct VirtReg {
    pub id: usize,
}

impl fmt::Display for VirtReg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "_t{}", self.id)
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum MemoryAddress {
    VirtReg(VirtReg),
    DefinedByte { virt_reg: VirtReg },
}

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub enum Immediate {
    I64(i64),
}

impl fmt::Display for Immediate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Immediate::I64(i) => write!(f, "{}", i),
        }
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum VirtRegArg {
    MemoryAddress(MemoryAddress),
    Immediate(Immediate),
}

impl fmt::Display for VirtRegArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VirtRegArg::Immediate(i) => write!(f, "{}", i),
            VirtRegArg::MemoryAddress(m) => match m {
                MemoryAddress::VirtReg(v) => write!(f, "{}", v.id),
                MemoryAddress::DefinedByte { virt_reg } => write!(f, "{}", virt_reg.id),
            },
        }
    }
}

#[derive(Debug)]
pub enum TACValue {
    Label {
        name: String,
    },
    BeginFunction {
        stack_bytes_needed: usize,
    },
    EndFunction,
    Double {
        target: VirtReg,
        value: VirtRegArg,
    },
    Quad {
        target: VirtReg,
        v1: VirtReg,
        op: Operation,
        v2: VirtReg,
    },
    PushDefinedByte {
        virt_reg: VirtReg,
        arg_num: usize,
    },
    PushIntLiteral {
        value: i64,
        arg_num: usize,
    },
    PushVirtReg {
        virt_reg: VirtReg,
        arg_num: usize,
    },
    Call {
        func_name: String,
    },
}

impl fmt::Display for TACValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TACValue::Label { name } => write!(f, "{}:", name),
            TACValue::BeginFunction { stack_bytes_needed } => {
                write!(f, "BeginFunction {}", stack_bytes_needed)
            }
            TACValue::EndFunction => write!(f, "EndFunction"),
            TACValue::Double { target, value } => write!(f, "{} = {}", target, value),
            TACValue::Quad { target, v1, op, v2 } => {
                write!(f, "{} = {} {} {}", target, v1, op, v2)
            }
            TACValue::PushDefinedByte { virt_reg, arg_num } => {
                write!(f, "PushArg {}, ({})", arg_num, virt_reg)
            }
            TACValue::PushIntLiteral { value, arg_num } => {
                write!(f, "PushArg {}, (${})", arg_num, value)
            }
            TACValue::PushVirtReg { virt_reg, arg_num } => {
                write!(f, "PushArg {}, ({})", arg_num, virt_reg)
            }
            TACValue::Call { func_name } => write!(f, "Call {}", func_name),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TACLiteral {
    String { virt_reg: VirtReg, value: String },
}

impl fmt::Display for TACLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TACLiteral::String { virt_reg, value } => write!(f, "{} = \"{}\"", virt_reg, value),
        }
    }
}

pub struct TAC {
    pub code: Vec<TACValue>,
    pub large_literals: Vec<TACLiteral>,
    curr_reg_id: usize,
    pub var_locations: HashMap<VirtReg, VirtRegArg>,
}

impl TAC {
    fn new() -> Self {
        Self {
            code: vec![],
            large_literals: vec![],
            curr_reg_id: 0,
            var_locations: HashMap::new(),
        }
    }

    pub fn generate(parser: &mut Parser) -> Self {
        let mut tac = TAC::new();

        match &parser.ast.node {
            Node::Function(f) => {
                // Handle args here

                tac.code.push(TACValue::Label {
                    name: f.name.clone(),
                });

                for statement in &f.body {
                    tac.generate_function_call(statement);
                }

                // Handle return here
            }
        };

        tac
    }

    fn generate_function_call(&mut self, statement: &Statement) {
        match statement {
            Statement::FunctionCall(fc) => {
                self.code.push(TACValue::BeginFunction {
                    stack_bytes_needed: fc.stack_bytes_needed,
                });

                let mut argc = 0;
                for arg in &fc.args {
                    argc += 1;
                    match arg {
                        Argument::Expression(e) => {
                            let mut tac_list = vec![];
                            let expr_reg = self.generate_expression(&mut tac_list, e);
                            for line in tac_list {
                                self.code.push(line);
                            }
                            self.code.push(TACValue::PushVirtReg {
                                virt_reg: expr_reg,
                                arg_num: argc,
                            });
                        }
                        Argument::String(s) => {
                            let newlines = s.matches("\\").count(); // may not always work
                            self.code.push(TACValue::PushIntLiteral {
                                value: (s.len() - newlines) as i64,
                                arg_num: argc,
                            });
                            let virt_reg = self.new_virt_reg();
                            argc += 1;

                            // db string
                            self.large_literals.push(TACLiteral::String {
                                virt_reg,
                                value: s.to_string(),
                            });
                            self.code.push(TACValue::PushDefinedByte {
                                virt_reg: virt_reg,
                                arg_num: argc,
                            });
                        }
                    }
                }

                self.code.push(TACValue::Call {
                    func_name: fc.name.clone(),
                });
                self.code.push(TACValue::EndFunction);
            }
            Statement::If(i) => todo!(),
        }
    }

    fn generate_expression(&mut self, tac_list: &mut Vec<TACValue>, expr: &Expression) -> VirtReg {
        let virt_reg = self.new_virt_reg();

        let t1 = if let Some(left) = &expr.left {
            match left.value {
                ExpressionValue::I64(i) => self.generate_immediate(tac_list, Immediate::I64(i)),
                ExpressionValue::Operation(_) => self.generate_expression(tac_list, left),
            }
        } else {
            unreachable!();
        };
        let t2 = if let Some(right) = &expr.right {
            match right.value {
                ExpressionValue::I64(i) => self.generate_immediate(tac_list, Immediate::I64(i)),
                ExpressionValue::Operation(_) => self.generate_expression(tac_list, right),
            }
        } else {
            unreachable!();
        };

        match &expr.value {
            ExpressionValue::I64(_) => unreachable!(),
            ExpressionValue::Operation(o) => {
                self.var_locations.insert(
                    virt_reg,
                    VirtRegArg::MemoryAddress(MemoryAddress::VirtReg(virt_reg)),
                );
                tac_list.push(TACValue::Quad {
                    target: virt_reg.clone(),
                    v1: t1,
                    op: o.clone(),
                    v2: t2,
                });
            }
        }

        virt_reg
    }

    fn generate_immediate(&mut self, tac_list: &mut Vec<TACValue>, val: Immediate) -> VirtReg {
        let virt_reg = self.new_virt_reg();
        self.var_locations
            .insert(virt_reg, VirtRegArg::Immediate(val));

        match val {
            Immediate::I64(_) => {
                tac_list.push(TACValue::Double {
                    target: virt_reg.clone(),
                    value: VirtRegArg::Immediate(val),
                });
            }
        }

        virt_reg
    }

    fn new_virt_reg(&mut self) -> VirtReg {
        let virt_reg = VirtReg {
            id: self.curr_reg_id,
        };
        self.curr_reg_id += 1;
        virt_reg
    }
}
