use crate::parser::*;
use crate::tac::*;
use std::collections::HashMap;

struct Rsp {
    offset: usize,
    func_begin: usize,
}

impl Rsp {
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

pub struct Asm {
    pub text_output: Vec<String>,
    pub data_output: Vec<String>,
    var_table: HashMap<VirtReg, usize>, // Register to stack offset
    rsp: Rsp,
}

impl Asm {
    pub fn generate(tac: &mut Tac) -> Self {
        let mut asm = Self {
            text_output: vec![],
            data_output: vec![],
            var_table: HashMap::new(),
            rsp: Rsp::new(),
        };

        asm.text_output.push(".global _start".to_string());

        for tacv in &tac.code {
            match tacv {
                TacValue::Label(l) => {
                    if l.name == "main" {
                        asm.text_output.push("_start:".to_string());
                    } else {
                        asm.text_output.push(format!("{}:", l));
                    }
                }
                TacValue::BeginFunction { stack_bytes_needed } => {
                    asm.text_output
                        .push(format!("sub ${}, %rsp", stack_bytes_needed));
                    asm.rsp.begin(*stack_bytes_needed);
                }
                TacValue::EndFunction => {
                    asm.text_output
                        .push(format!("add ${}, %rsp", asm.rsp.func_begin));
                    asm.rsp.end();
                }
                TacValue::Double(d) => {
                    // TODO: Support immediates that are larger than 32 bits
                    let offset = asm.rsp.push(8);
                    asm.var_table.insert(d.target, offset);
		    asm.text_output.push(format!(
			"movq ${}, -{}(%rsp)",
			tac.var_locations.get(&d.target).unwrap(),
			offset
		    ));
                }
                TacValue::Quad(q) => {
                    asm.generate_quad(q);
                }
                TacValue::PushVirtReg { virt_reg, arg_num } => {
                    let offset = asm.var_table.get(virt_reg).unwrap();
                    match arg_num {
                        1 => asm.text_output.push(format!("mov -{}(%rsp), %rdi", offset)),
                        2 => asm.text_output.push(format!("mov -{}(%rsp), %rsi", offset)),
                        _ => todo!(),
                    }
                }
                TacValue::PushDefinedByte { label, arg_num } => {
                    let offset = asm.rsp.push(8);
                    asm.text_output
                        .push(format!("lea .L{}(%rip), %r10", label.name));
                    asm.text_output
                        .push(format!("movq %r10, -{}(%rsp)", offset));
                    match arg_num {
                        1 => asm.text_output.push(format!("mov -{}(%rsp), %rdi", offset)),
                        2 => asm.text_output.push(format!("mov -{}(%rsp), %rsi", offset)),
                        _ => todo!(),
                    }
                }
                TacValue::PushIntLiteral { value, arg_num } => {
                    let offset = asm.rsp.push(8);
                    asm.text_output
                        .push(format!("movq ${}, -{}(%rsp)", value, offset));
                    match arg_num {
                        1 => asm.text_output.push(format!("mov -{}(%rsp), %rdi", offset)),
                        2 => asm.text_output.push(format!("mov -{}(%rsp), %rsi", offset)),
                        _ => todo!(),
                    }
                }
                TacValue::Call { func_name } => {
                    if func_name == "print_string" {
                        asm.text_output.push(".extern _la_print_string".to_string());
                        asm.text_output.push("call _la_print_string".to_string());
                    } else if func_name == "print_int" {
                        asm.text_output.push(".extern _la_print_i64".to_string());
                        asm.text_output.push("call _la_print_i64".to_string());
                    } else {
                        asm.text_output.push(format!("call _{}", func_name));
                    }
                }
                TacValue::DefineStringLiteral { label, value } => {
                    asm.data_output.push(format!(".L{}:", label.name.clone()));
                    asm.data_output.push(format!(".ascii \"{}\"", value));
                }
                TacValue::IfZero { virt_reg, goto } => {
                    asm.text_output.push(format!("jz {}", goto.dest));
                }
                TacValue::Goto(g) => {
                    asm.text_output.push(format!("jmp {}", g.dest));
                }
            }
        }

        asm.text_output.push("mov $60, %rax".to_string()); // sys_exit
        asm.text_output.push("mov $0, %rdi".to_string()); // return code
        asm.text_output.push("syscall".to_string());

        asm
    }

    pub fn generate_quad(&mut self, q: &Quad) {
        let offset = self.rsp.push(8);
        match q.op {
            Operation::Add => {
                self.add(&q.v1, &q.v2);
                self.text_output
                    .push(format!("mov %rax, -{}(%rsp)", offset));
            }
            Operation::Sub => {
                self.sub(&q.v1, &q.v2);
                self.text_output
                    .push(format!("mov %rax, -{}(%rsp)", offset));
            }
            Operation::Mul => {
                self.mul(&q.v1, &q.v2);
                self.text_output
                    .push(format!("mov %rax, -{}(%rsp)", offset));
            }
            Operation::Div => {
                self.div(&q.v1, &q.v2);
                self.text_output
                    .push(format!("mov %rax, -{}(%rsp)", offset));
            }
            Operation::EqEq => {
                self.eqeq(&q.v1, &q.v2);
                self.text_output
                    .push(format!("mov %rax, -{}(%rsp)", offset));
            }
        };
        self.var_table.insert(q.target, offset);
    }

    pub fn add(&mut self, v1: &VirtReg, v2: &VirtReg) {
        self.text_output.push(format!(
            "mov {}(%rsp), %rax",
            -(*self.var_table.get(v1).unwrap() as i32) + self.rsp.func_begin as i32
        ));
        self.text_output.push(format!(
            "mov {}(%rsp), %rbx",
            -(*self.var_table.get(v2).unwrap() as i32) + self.rsp.func_begin as i32
        ));
        self.text_output.push("add %rbx, %rax".to_string());
    }

    pub fn sub(&mut self, v1: &VirtReg, v2: &VirtReg) {
        self.text_output.push(format!(
            "mov {}(%rsp), %rax",
            -(*self.var_table.get(v1).unwrap() as i32) + self.rsp.func_begin as i32
        ));
        self.text_output.push(format!(
            "mov {}(%rsp), %rbx",
            -(*self.var_table.get(v2).unwrap() as i32) + self.rsp.func_begin as i32
        ));
        self.text_output.push("sub %rbx, %rax".to_string());
    }

    pub fn mul(&mut self, v1: &VirtReg, v2: &VirtReg) {
        self.text_output.push(format!(
            "mov {}(%rsp), %rax",
            -(*self.var_table.get(v1).unwrap() as i32) + self.rsp.func_begin as i32
        ));
        self.text_output.push(format!(
            "mov {}(%rsp), %rbx",
            -(*self.var_table.get(v2).unwrap() as i32) + self.rsp.func_begin as i32
        ));
        self.text_output.push("imul %rbx, %rax".to_string());
    }

    pub fn div(&mut self, v1: &VirtReg, v2: &VirtReg) {
        self.text_output.push(format!(
            "mov {}(%rsp), %rax",
            -(*self.var_table.get(v1).unwrap() as i32) + self.rsp.func_begin as i32
        ));
        self.text_output.push(format!(
            "mov {}(%rsp), %rbx", // Divisor
            -(*self.var_table.get(v2).unwrap() as i32) + self.rsp.func_begin as i32
        ));
        self.text_output.push("mov $0, %rdx".to_string()); // Clear high bits of dividend
        self.text_output.push("cqto".to_string()); // Sign extend rax to rdx:rax
        self.text_output.push("idiv %rbx".to_string());
    }

    pub fn eqeq(&mut self, v1: &VirtReg, v2: &VirtReg) {
        self.text_output.push(format!(
            "mov {}(%rsp), %rax",
            -(*self.var_table.get(v1).unwrap() as i32) + self.rsp.func_begin as i32
        ));
        self.text_output.push(format!(
            "mov {}(%rsp), %rbx",
            -(*self.var_table.get(v2).unwrap() as i32) + self.rsp.func_begin as i32
        ));
        self.text_output.push("cmp %rbx, %rax".to_string());
        self.text_output.push("sete %al".to_string()); // Set al to 1 if equal, 0 otherwise
        self.text_output.push("movzx %al, %rax".to_string()); // Copy al to rax
    }
}
