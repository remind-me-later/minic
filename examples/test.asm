.section .text
.globl _start


_start:
	movq %rsp, %rbp
	subq $0, %rsp
main_entry:
	pushq $6
	call fact
	popq %rbx
	pushq %rax
	call print_int
	popq %rbx
	pushq $10
	call print_char
	popq %rbx
	movq $60, %rax
	xorq %rdi, %rdi
	syscall
print_int:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
print_int_entry:
	pushq $0
	popq %rax
	movq %rax, -8(%rbp)
	movq 16(%rbp), %rax
	pushq %rax
	pushq $10
	popq %rbx
	popq %rax
	cmpq %rbx, %rax
	jl if_then_0
	jmp if_else_1
if_then_0:
	pushq $48
	movq 16(%rbp), %rax
	pushq %rax
	popq %rbx
	popq %rax
	addq %rbx, %rax
	pushq %rax
	call print_char
	popq %rbx
	jmp if_end_2
if_else_1:
	movq 16(%rbp), %rax
	pushq %rax
	pushq $10
	popq %rbx
	popq %rax
	cqo
	idivq %rbx
	pushq %rax
	call print_int
	popq %rbx
	movq 16(%rbp), %rax
	pushq %rax
	pushq $10
	popq %rbx
	popq %rax
	cqo
	idivq %rbx
	pushq %rdx
	popq %rax
	movq %rax, -8(%rbp)
	pushq $48
	movq -8(%rbp), %rax
	pushq %rax
	popq %rbx
	popq %rax
	addq %rbx, %rax
	pushq %rax
	call print_char
	popq %rbx
	jmp if_end_2
if_end_2:
	movq %rbp, %rsp
	popq %rbp
	ret
fact:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
fact_entry:
	movq 16(%rbp), %rax
	pushq %rax
	pushq $1
	popq %rbx
	popq %rax
	cmpq %rbx, %rax
	jle if_then_3
	jmp if_end_4
if_then_3:
	pushq $1
	popq %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	jmp if_end_4
if_end_4:
	movq 16(%rbp), %rax
	pushq %rax
	movq 16(%rbp), %rax
	pushq %rax
	pushq $1
	popq %rbx
	popq %rax
	subq %rbx, %rax
	pushq %rax
	call fact
	popq %rbx
	pushq %rax
	popq %rbx
	popq %rax
	imulq %rbx, %rax
	pushq %rax
	popq %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	movq %rbp, %rsp
	popq %rbp
	ret
