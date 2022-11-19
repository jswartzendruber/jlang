use crate::parser::*;

#[derive(Debug)]
pub struct VirtReg {
    id: usize,
}

#[derive(Debug)]
pub enum MemoryAddress {
    VirtReg(VirtReg),
    DefinedByte { id: usize },
}

#[derive(Debug)]
pub enum Immediate {
    I64(i64),
}

#[derive(Debug)]
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
}

impl TAC {
    fn new() -> Self {
	Self { code: vec![], large_literals: vec![], curr_reg_id: 0 }
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
			    todo!();
			},
			Argument::String(s) => {
			    let newlines = s.matches("\\n").count();
			    self.code.push(TACValue::PushIntLiteral { value: (s.len() - newlines) as i64, arg_num: argc } );
			    self.curr_reg_id += 1;
			    argc += 1;

			    // db string
			    self.large_literals.push(
				TACLiteral::String {
				    id: self.curr_reg_id,
				    value: s.to_string()
				}
			    );
			    self.code.push(TACValue::PushDefinedByte { param_id: self.curr_reg_id, arg_num: argc } );
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
}
