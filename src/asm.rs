use crate::tac::*;
use std::collections::HashMap;

pub struct ASM {
    pub data_output: Vec<String>,
    pub text_output: Vec<String>,
    vars: HashMap<usize, TACLiteral>,
}

impl ASM {
    pub fn generate(tac: &mut TAC) -> Self {
	let mut asm = Self {
	    data_output: vec![],
	    text_output: vec![],
	    vars: HashMap::new()
	};

	asm.text_output.push(".global _start".to_string());

	for literal in &tac.large_literals {
	    match literal { // Need to convert newline to ascii?
		TACLiteral::String { id, value } => {
		    asm.data_output.push(format!("._t{}:", id));
		    asm.data_output.push(format!(".ascii \"{}\"", value));
		    asm.vars.insert(*id, literal.clone());
		},
	    };
	}

	for tacv in &tac.code {
	    match tacv {
		TACValue::Label { name } => {
		    if name == "main" {
			asm.text_output.push("_start:".to_string());
		    } else {
			asm.text_output.push(format!("_{}:", name));
		    }
		},
		TACValue::BeginFunction { stack_bytes_needed } => asm.text_output.push(format!("sub ${}, %rsp", stack_bytes_needed)),
		TACValue::EndFunction => asm.text_output.push("pop %rbp".to_string()),
		TACValue::Double { target, value } => todo!(),
		TACValue::PushDefinedByte { param_id, arg_num } => {
		    asm.text_output.push(format!("lea ._t{}(%rip), %r10", param_id));
		    asm.text_output.push(format!("movq %r10, -{}(%rsp)", arg_num * 8));
		    match arg_num {
			1 => asm.text_output.push(format!("mov -{}(%rsp), %rdi", arg_num * 8)),
			2 => asm.text_output.push(format!("mov -{}(%rsp), %rsi", arg_num * 8)),
			_ => todo!(),
		    }
		},
		TACValue::PushIntLiteral { value, arg_num } => {
		    asm.text_output.push(format!("movq ${}, -{}(%rsp)", value, arg_num * 8));
		    match arg_num {
			1 => asm.text_output.push(format!("mov -{}(%rsp), %rdi", arg_num * 8)),
			2 => asm.text_output.push(format!("mov -{}(%rsp), %rsi", arg_num * 8)),
			_ => todo!(),
		    }
		},
		TACValue::Call { func_name } => {
		    if func_name == "print_string" {
			asm.text_output.push(".extern _la_print_string".to_string());
			asm.text_output.push("call _la_print_string".to_string());
		    } else {
			asm.text_output.push(format!("call _{}", func_name));
		    }
		},
		TACValue::Pop { bytes_to_pop } => {
		    asm.text_output.push(format!("add ${}, %rsp", bytes_to_pop));
		}
	    }
	}

	asm.text_output.push("mov $60, %rax".to_string()); // sys_exit
	asm.text_output.push("mov $0, %rdi".to_string()); // return code
	asm.text_output.push("syscall".to_string());

	asm
    }
}
