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
pub enum TacValue {
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

impl fmt::Display for TacValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TacValue::Label { name } => write!(f, "{}:", name),
            TacValue::BeginFunction { stack_bytes_needed } => {
                write!(f, "BeginFunction {}", stack_bytes_needed)
            }
            TacValue::EndFunction => write!(f, "EndFunction"),
            TacValue::Double { target, value } => write!(f, "{} = {}", target, value),
            TacValue::Quad { target, v1, op, v2 } => {
                write!(f, "{} = {} {} {}", target, v1, op, v2)
            }
            TacValue::PushDefinedByte { virt_reg, arg_num } => {
                write!(f, "PushArg {}, ({})", arg_num, virt_reg)
            }
            TacValue::PushIntLiteral { value, arg_num } => {
                write!(f, "PushArg {}, (${})", arg_num, value)
            }
            TacValue::PushVirtReg { virt_reg, arg_num } => {
                write!(f, "PushArg {}, ({})", arg_num, virt_reg)
            }
            TacValue::Call { func_name } => write!(f, "Call {}", func_name),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TacLiteral {
    String { virt_reg: VirtReg, value: String },
}

impl fmt::Display for TacLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TacLiteral::String { virt_reg, value } => write!(f, "{} = \"{}\"", virt_reg, value),
        }
    }
}

pub struct Tac {
    pub code: Vec<TacValue>,
    pub large_literals: Vec<TacLiteral>,
    curr_reg_id: usize,
    pub var_locations: HashMap<VirtReg, VirtRegArg>,
}

impl Tac {
    fn new() -> Self {
        Self {
            code: vec![],
            large_literals: vec![],
            curr_reg_id: 0,
            var_locations: HashMap::new(),
        }
    }

    pub fn generate(parser: &mut Parser) -> Self {
        let mut tac = Tac::new();

        match &parser.ast.node {
            Node::Function(f) => {
                // Handle args here

                tac.code.push(TacValue::Label {
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
                self.code.push(TacValue::BeginFunction {
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
                            self.code.push(TacValue::PushVirtReg {
                                virt_reg: expr_reg,
                                arg_num: argc,
                            });
                        }
                        Argument::String(s) => {
                            let newlines = s.matches('\\').count(); // may not always work
                            self.code.push(TacValue::PushIntLiteral {
                                value: (s.len() - newlines) as i64,
                                arg_num: argc,
                            });
                            let virt_reg = self.new_virt_reg();
                            argc += 1;

                            // db string
                            self.large_literals.push(TacLiteral::String {
                                virt_reg,
                                value: s.to_string(),
                            });
                            self.code.push(TacValue::PushDefinedByte {
                                virt_reg,
                                arg_num: argc,
                            });
                        }
                    }
                }

                self.code.push(TacValue::Call {
                    func_name: fc.name.clone(),
                });
                self.code.push(TacValue::EndFunction);
            }
            Statement::If(_i) => todo!(),
        }
    }

    fn generate_expression(&mut self, tac_list: &mut Vec<TacValue>, expr: &Expression) -> VirtReg {
        let virt_reg = self.new_virt_reg();

        match &expr.value {
            ExpressionValue::I64(i) => {
                self.generate_immediate(tac_list, Immediate::I64(*i), virt_reg);
            }
            ExpressionValue::Operation(o) => {
                let t1 = match expr.left.as_ref().unwrap().value {
                    ExpressionValue::I64(i) => {
			let vr = self.new_virt_reg();
                        self.generate_immediate(tac_list, Immediate::I64(i), vr)
                    }
                    ExpressionValue::Operation(_) => {
                        self.generate_expression(tac_list, expr.left.as_ref().unwrap())
                    }
                };

                let t2 = match expr.right.as_ref().unwrap().value {
                    ExpressionValue::I64(i) => {
			let vr = self.new_virt_reg();
                        self.generate_immediate(tac_list, Immediate::I64(i), vr)
                    }
                    ExpressionValue::Operation(_) => {
                        self.generate_expression(tac_list, expr.right.as_ref().unwrap())
                    }
                };

                self.var_locations.insert(
                    virt_reg,
                    VirtRegArg::MemoryAddress(MemoryAddress::VirtReg(virt_reg)),
                );
                tac_list.push(TacValue::Quad {
                    target: virt_reg,
                    v1: t1,
                    op: o.clone(),
                    v2: t2,
                });
            }
        }

        virt_reg
    }

    fn generate_immediate(
        &mut self,
        tac_list: &mut Vec<TacValue>,
        val: Immediate,
        virt_reg: VirtReg,
    ) -> VirtReg {
        self.var_locations
            .insert(virt_reg, VirtRegArg::Immediate(val));

        match val {
            Immediate::I64(_) => {
                tac_list.push(TacValue::Double {
                    target: virt_reg,
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
