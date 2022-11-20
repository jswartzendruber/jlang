use crate::parser::*;
use crate::tac::*;
use std::collections::HashMap;

struct RSP {
    offset: usize,
    func_begin: usize,
}

impl RSP {
    fn new() -> Self {
        Self {
            offset: 0,
            func_begin: 0,
        }
    }
    fn push(&mut self, size: usize) -> usize {
        self.offset += size;
        self.offset
    }
    fn pop(&mut self, size: usize) -> usize {
        self.offset -= size;
        self.offset
    }
    fn begin(&mut self, func_begin: usize) {
        self.func_begin = func_begin
    }
    fn end(&mut self) {
        self.pop(self.func_begin);
        self.func_begin = 0;
    }
}

pub struct ASM {
    pub data_output: Vec<String>,
    pub text_output: Vec<String>,
    var_table: HashMap<VirtReg, usize>,
    rsp: RSP,
}

impl ASM {
    pub fn generate(tac: &mut TAC) -> Self {
        let mut asm = Self {
            data_output: vec![],
            text_output: vec![],
            var_table: HashMap::new(),
            rsp: RSP::new(),
        };

        asm.text_output.push(".global _start".to_string());

        for literal in &tac.large_literals {
            match literal {
                TACLiteral::String { virt_reg, value } => {
                    asm.data_output.push(format!("._t{}:", virt_reg.id));
                    asm.data_output.push(format!(".ascii \"{}\"", value));
                    // asm.var_table.insert(*virt_reg, literal.clone());
                }
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
                }
                TACValue::BeginFunction { stack_bytes_needed } => {
                    asm.text_output
                        .push(format!("sub ${}, %rsp", stack_bytes_needed));
                    asm.rsp.begin(*stack_bytes_needed);
                }
                TACValue::EndFunction => {
                    asm.text_output
                        .push(format!("add ${}, %rsp", asm.rsp.func_begin));
                    asm.rsp.end();
                }
                TACValue::Double { target, .. } => {
                    let offset = asm.rsp.push(8);
                    asm.var_table.insert(*target, offset);
                    asm.text_output.push(format!(
                        "movq ${}, -{}(%rsp)",
                        tac.var_locations.get(&target).unwrap(),
                        offset
                    ));
                }
                TACValue::Quad { target, v1, op, v2 } => {
                    let offset = asm.rsp.push(8);
                    match op {
                        Operation::Add => {
                            asm.text_output.push(format!(
                                "mov -{}(%rsp), %rax",
                                asm.var_table.get(&v1).unwrap()
                            ));
                            asm.text_output.push(format!(
                                "add -{}(%rsp), %rbx",
                                asm.var_table.get(&v2).unwrap()
                            ));
                            asm.text_output.push(format!("add %rbx, %rax"));
                            asm.text_output.push(format!("mov %rax, -{}(%rsp)", offset));
                        }
                        Operation::Sub => todo!(),
                        Operation::Mul => todo!(),
                        Operation::Div => todo!(),
                    };
                    asm.var_table.insert(*target, offset);
                }
                TACValue::PushVirtReg { virt_reg, arg_num } => {
                    let offset = asm.var_table.get(&virt_reg).unwrap();
                    match arg_num {
                        1 => asm.text_output.push(format!("mov -{}(%rsp), %rdi", offset)),
                        2 => asm.text_output.push(format!("mov -{}(%rsp), %rsi", offset)),
                        _ => todo!(),
                    }
                }
                TACValue::PushDefinedByte { virt_reg, arg_num } => {
                    let offset = asm.rsp.push(8);
                    asm.text_output
                        .push(format!("lea ._t{}(%rip), %r10", virt_reg.id));
                    asm.text_output
                        .push(format!("movq %r10, -{}(%rsp)", offset));
                    match arg_num {
                        1 => asm.text_output.push(format!("mov -{}(%rsp), %rdi", offset)),
                        2 => asm.text_output.push(format!("mov -{}(%rsp), %rsi", offset)),
                        _ => todo!(),
                    }
                }
                TACValue::PushIntLiteral { value, arg_num } => {
                    let offset = asm.rsp.push(8);
                    asm.text_output
                        .push(format!("movq ${}, -{}(%rsp)", value, offset));
                    match arg_num {
                        1 => asm.text_output.push(format!("mov -{}(%rsp), %rdi", offset)),
                        2 => asm.text_output.push(format!("mov -{}(%rsp), %rsi", offset)),
                        _ => todo!(),
                    }
                }
                TACValue::Call { func_name } => {
                    if func_name == "print_string" {
                        asm.text_output.push(".extern _la_print_string".to_string());
                        asm.text_output.push("call _la_print_string".to_string());
                    } else if func_name == "print_int" {
                        asm.text_output.push(".extern _la_print_u64".to_string());
                        asm.text_output.push("call _la_print_u64".to_string());
                    } else {
                        asm.text_output.push(format!("call _{}", func_name));
                    }
                }
            }
        }

        asm.text_output.push("mov $60, %rax".to_string()); // sys_exit
        asm.text_output.push("mov $0, %rdi".to_string()); // return code
        asm.text_output.push("syscall".to_string());

        asm
    }
}
