use crate::parser::*;
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct VirtReg {
    id: usize,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum MemoryAddress {
    VirtReg(VirtReg),
    DefinedByte { id: usize },
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Immediate {
    I64(i64),
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum VirtRegArg {
    MemoryAddress(MemoryAddress),
    Immediate(Immediate),
}

#[derive(Debug)]
pub enum TACValue {
    Label { name: String },
    BeginFunction { stack_bytes_needed: usize },
    EndFunction,
    Double { target: VirtReg, value: VirtRegArg },
    Quad { target: VirtReg, v1: VirtReg, op: Operation, v2: VirtReg },
    PushDefinedByte { param_id: usize, arg_num: usize },
    PushIntLiteral { value: i64, arg_num: usize },
    Call { func_name: String },
    Pop { bytes_to_pop: usize },
}

#[derive(Debug, Clone)]
pub enum TACLiteral {
    String { id: usize, value: String },
}

pub struct TAC {
    pub code: Vec<TACValue>,
    pub large_literals: Vec<TACLiteral>,
    curr_reg_id: usize,
    var_locations: HashMap<Immediate, VirtReg>
}

impl TAC {
    fn new() -> Self {
	Self { code: vec![], large_literals: vec![], curr_reg_id: 0, var_locations: HashMap::new() }
    }

    pub fn generate(parser: &mut Parser) -> Self {
	let mut tac = TAC::new();

	match &parser.ast.node {
	    Node::Function(f) => {
		// Handle args here

		tac.code.push(TACValue::Label { name: f.name.clone() });

		for statement in &f.body {
		    tac.generate_function_call(statement);
		}

		// Handle return here
	    },
	};

	tac
    }

    fn generate_function_call(&mut self, statement: &Statement) {
	match statement {
	    Statement::FunctionCall(fc) => {
		self.code.push(TACValue::BeginFunction { stack_bytes_needed: fc.stack_bytes_needed } );

		let mut argc = 0;
		for arg in &fc.args {
		    argc += 1;
		    match arg {
			Argument::Expression(e) => {
			    let mut tac_list = vec![];
			    self.generate_expression(&mut tac_list, e);
			    for line in tac_list {
				self.code.push(line);
			    }
			},
			Argument::String(s) => {
			    let newlines = s.matches("\\n").count(); // just have \\ ?
			    self.code.push(TACValue::PushIntLiteral {
				value: (s.len() - newlines) as i64,
				arg_num: argc
			    });
			    self.curr_reg_id += 1;
			    argc += 1;

			    // db string
			    self.large_literals.push(
				TACLiteral::String {
				    id: self.curr_reg_id,
				    value: s.to_string()
				}
			    );
			    self.code.push(TACValue::PushDefinedByte {
				param_id: self.curr_reg_id,
				arg_num: argc
			    });
			    self.curr_reg_id += 1;
			}
		    }
		}

		self.code.push(TACValue::Call { func_name: fc.name.clone() } );
		self.code.push(TACValue::Pop { bytes_to_pop: fc.stack_bytes_needed } );
		self.code.push(TACValue::EndFunction);
	    }
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
		tac_list.push(TACValue::Quad { target: virt_reg.clone(), v1: t1, op: o.clone(), v2: t2 } );
	    }
	}

	virt_reg
    }

    fn generate_immediate(&mut self, tac_list: &mut Vec<TACValue>, val: Immediate) -> VirtReg {
	let virt_reg = self.new_virt_reg();

	match val {
	    Immediate::I64(_) => {
		tac_list.push(TACValue::Double {
		    target: virt_reg.clone(),
		    value: VirtRegArg::Immediate(val)
		});
	    }
	}

	virt_reg
    }

    fn new_virt_reg(&mut self) -> VirtReg {
	let virt_reg = VirtReg { id: self.curr_reg_id };
	self.curr_reg_id += 1;
	virt_reg
    }
}

