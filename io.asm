	.global _la_print_string

# rdi - int length
# rsi - char* message
_la_print_string:
	mov %rdi, %rdx
	mov %rsi, %rcx
	mov $1, %rax # sys_write
	mov $1, %rdi # stdout
	syscall
	ret
