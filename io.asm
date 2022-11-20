	.global _la_print_string
	.global _la_print_char
	.global _la_print_i64

# rdi - int length
# rsi - char* message
_la_print_string:
	mov %rdi, %rdx
	mov %rsi, %rcx
	mov $1, %rax # sys_write
	mov $1, %rdi # stdout
	syscall
	ret

# rdi - i64 i
_la_print_i64:
	cmp $0, %rdi
	jns _la_print_i64_0
	push %rdi
	mov $0x2D, %rdi
	call _la_print_char
	pop %rdi
	neg %rdi
_la_print_i64_0:
	mov $0, %r10
	mov %rdi, %rax # Dividend
_la_print_i64_1:
	mov $10, %rcx # Divisor
	mov $0, %rdx # High bits of dividend
	cqto # sign extend rax to rdx:rax
	idiv %rcx # rax /= rcx

	push %rdx
	inc %r10

	cmp $0, %rax
	jg _la_print_i64_1 # push chars on stack until i is 0
_la_print_i64_2:
	pop %rdi
	add $0x30, %rdi # convert to char
	call _la_print_char
	dec %r10

	cmp $0, %r10
	jg _la_print_i64_2 # print all chars on stack
	ret

# rdi - char c
_la_print_char:
	push %rdi
	mov %rsp, %rsi # char *
	mov $1, %rdx # len
	mov $1, %rax # sys_write
	mov $1, %rdi # stdout
	syscall
	pop %rdi
	ret
