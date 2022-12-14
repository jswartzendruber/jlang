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

#[derive(Debug, Clone)]
pub struct Label {
    pub name: String,
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, ".L{}", self.name)
    }
}

impl Label {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum VirtRegArg {
    Immediate(i64),
}

impl fmt::Display for VirtRegArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VirtRegArg::Immediate(i) => write!(f, "{}", i),
        }
    }
}

#[derive(Debug)]
pub struct Double {
    pub target: VirtReg,
    value: VirtRegArg,
}

impl Double {
    fn new(target: VirtReg, value: VirtRegArg) -> Self {
        Self { target, value }
    }
}

#[derive(Debug)]
pub struct Quad {
    pub target: VirtReg,
    pub v1: VirtReg,
    pub op: Operation,
    pub v2: VirtReg,
}

impl Quad {
    fn new(target: VirtReg, v1: VirtReg, op: Operation, v2: VirtReg) -> Self {
        Self { target, v1, op, v2 }
    }
}

#[derive(Debug)]
pub struct Goto {
    pub dest: Label,
}

impl fmt::Display for Goto {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Goto {}", self.dest)
    }
}

#[derive(Debug)]
pub enum TacValue {
    Label(Label),
    BeginFunction { stack_bytes_needed: usize },
    EndFunction,
    Double(Double),
    Quad(Quad),
    PushDefinedByte { label: Label, arg_num: usize },
    PushIntLiteral { value: i64, arg_num: usize },
    PushVirtReg { virt_reg: VirtReg, arg_num: usize },
    Call { func_name: String },
    DefineStringLiteral { label: Label, value: String },
    Goto(Goto),
    IfZero { virt_reg: VirtReg, goto: Goto },
}

impl fmt::Display for TacValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TacValue::Label(l) => write!(f, "{}", l),
            TacValue::BeginFunction { stack_bytes_needed } => {
                write!(f, "BeginFunction {}", stack_bytes_needed)
            }
            TacValue::EndFunction => write!(f, "EndFunction"),
            TacValue::Double(d) => write!(f, "{} = {}", d.target, d.value),
            TacValue::Quad(q) => {
                write!(f, "{} = {} {} {}", q.target, q.v1, q.op, q.v2)
            }
            TacValue::PushDefinedByte { label, arg_num } => {
                write!(f, "PushArg {}, ({})", arg_num, label)
            }
            TacValue::PushIntLiteral { value, arg_num } => {
                write!(f, "PushArg {}, (${})", arg_num, value)
            }
            TacValue::PushVirtReg { virt_reg, arg_num } => {
                write!(f, "PushArg {}, ({})", arg_num, virt_reg)
            }
            TacValue::Call { func_name } => write!(f, "Call {}", func_name),
            TacValue::DefineStringLiteral { label, value } => {
                write!(f, "{} \"{}\"", label, value)
            }
            TacValue::Goto(g) => write!(f, "{}", g),
            TacValue::IfZero { virt_reg, goto } => write!(f, "IfZero {}, {}", virt_reg, goto),
        }
    }
}

pub struct Tac {
    pub code: Vec<TacValue>,
    curr_reg_id: usize,
    pub var_map: HashMap<String, VirtReg>,
    file_contents: String,
}

impl Tac {
    fn new(file_contents: String) -> Self {
        Self {
            code: vec![],
            curr_reg_id: 0,
            var_map: HashMap::new(),
            file_contents,
        }
    }

    pub fn generate(parser: Parser) -> Self {
        let mut tac = Tac::new(parser.file_contents);

        match &parser.ast.unwrap().node {
            Node::Module(m) => {
                for func in &m.functions {
                    tac.generate_function(func);
                }
            }
        };

        tac
    }

    fn generate_function(&mut self, function: &Function) {
        // TODO: Handle function arguments

        self.code
            .push(TacValue::Label(Label::new(function.name.clone())));

        self.code.push(TacValue::BeginFunction {
            stack_bytes_needed: function.stack_bytes_needed,
        });

        for statement in &function.body.statements {
            self.generate_statement(statement, &function.body.scope);
        }

        // TODO: Handle function return type

        self.code.push(TacValue::EndFunction);
    }

    fn generate_statement(&mut self, statement: &Statement, scope: &Scope) {
        match statement {
	    Statement::Return(r) => {
		todo!();
	    }
            Statement::VarDeclaration(v) => match v.var_type {
                Type::I64 => {
                    let virt_reg = self.generate_expression(&v.value, scope);
                    self.var_map.insert(v.name.clone(), virt_reg);
                }
                _ => {
                    unimplemented!();
                }
            },
            Statement::FunctionCall(fc) => {
                self.code.push(TacValue::BeginFunction {
                    stack_bytes_needed: fc.stack_bytes_needed,
                });

                let mut argc = 0;
                for arg in &fc.args {
                    argc += 1;
                    match arg {
                        Argument::Expression(e) => {
                            let expr_reg = self.generate_expression(e, scope);
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
                            argc += 1;

                            let l1 = self.new_label();
                            self.code.push(TacValue::DefineStringLiteral {
                                label: l1.clone(),
                                value: s.to_string(),
                            });
                            self.code.push(TacValue::PushDefinedByte {
                                label: l1,
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
            Statement::If(i) => {
                let condition_reg = self.generate_expression(&i.condition, scope);
                let l1 = self.new_label();
                let l2 = self.new_label();

                self.code.push(TacValue::IfZero {
                    virt_reg: condition_reg,
                    goto: Goto { dest: l1.clone() },
                });

                // If not equal do this part
                if let Some(block) = &i.if_false {
                    for stmt in &block.statements {
                        self.generate_statement(stmt, &block.scope);
                    }
                }
                // Jump over if not equal part
                self.code.push(TacValue::Goto(Goto { dest: l2.clone() }));
                self.code.push(TacValue::Label(l1));

                // If equal do this part
                for stmt in &i.if_true.statements {
                    self.generate_statement(stmt, &i.if_true.scope);
                }
                self.code.push(TacValue::Label(l2));
            }
        }
    }

    fn generate_expression(&mut self, expr: &Expression, scope: &Scope) -> VirtReg {
        let virt_reg = self.new_virt_reg();

        match &expr.value {
            ExpressionValue::FunctionCall(fc, sc) => {
                println!("a");
                self.generate_statement(&Statement::FunctionCall(fc.clone()), sc);
            }
            ExpressionValue::Variable(v) => {
                let virt_reg = self.var_map.get(&v.name).unwrap();
                println!("virt {}", virt_reg);
            }
            ExpressionValue::I64(i) => {
                self.generate_immediate(VirtRegArg::Immediate(*i), virt_reg);
            }
            ExpressionValue::Operation(o) => {
                let t1 = self.generate_operation(expr.left.as_ref().unwrap(), scope);
                let t2 = self.generate_operation(expr.right.as_ref().unwrap(), scope);

                self.code
                    .push(TacValue::Quad(Quad::new(virt_reg, t1, o.clone(), t2)));
            }
        }

        virt_reg
    }

    fn generate_operation(&mut self, expr: &Expression, scope: &Scope) -> VirtReg {
        match &expr.value {
            ExpressionValue::FunctionCall(fc, sc) => {
                self.generate_statement(&Statement::FunctionCall(fc.clone()), sc);
                self.new_virt_reg() // this probably won't work
            }
            ExpressionValue::Variable(v) => *self.var_map.get(&v.name).unwrap(),
            ExpressionValue::I64(i) => {
                let vr = self.new_virt_reg();
                self.generate_immediate(VirtRegArg::Immediate(*i), vr)
            }
            ExpressionValue::Operation(_) => {
                self.generate_expression(expr.right.as_ref().unwrap(), scope)
            }
        }
    }

    fn generate_immediate(&mut self, val: VirtRegArg, virt_reg: VirtReg) -> VirtReg {
        match val {
            VirtRegArg::Immediate(i) => {
                self.code.push(TacValue::Double(Double::new(
                    virt_reg,
                    VirtRegArg::Immediate(i),
                )));
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

    fn new_label(&mut self) -> Label {
        self.curr_reg_id += 1;
        Label::new(format!("{}", self.curr_reg_id - 1))
    }
}
