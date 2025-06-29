.section .text
.globl _start

.extern print_char
.type print_char, @function

_start:
	movq %rsp, %rbp
	subq $24, %rsp
main_entry:
	pushq $10
	popq %rax
	movq %rax, (%rbp)
	pushq $0
	popq %rax
	movq %rax, -8(%rbp)
	pushq $5
	call fact
	popq %rbx
	pushq %rax
	popq %rax
	movq %rax, -16(%rbp)
	pushq $20
	popq %rax
	movq %rax, -8(%rbp)
	movq (%rbp), %rax
	pushq %rax
	call print_int
	popq %rbx
	pushq $32
	call print_char
	popq %rbx
	movq -8(%rbp), %rax
	pushq %rax
	call print_int
	popq %rbx
	pushq $32
	call print_char
	popq %rbx
	movq -16(%rbp), %rax
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
	subq $16, %rsp
print_int_entry:
	pushq $0
	popq %rax
	movq %rax, -16(%rbp)
	movq 16(%rbp), %rax
	pushq %rax
	movq $10, %rbx
	popq %rax
	cmpq %rbx, %rax
	jl if_then_0
	jmp if_else_1
if_then_0:
	movq 16(%rbp), %rax
	pushq %rax
	movq $48, %rax
	popq %rbx
	addq %rbx, %rax
	pushq %rax
	call print_char
	popq %rbx
	jmp if_end_2
if_else_1:
	movq 16(%rbp), %rax
	pushq %rax
	movq $10, %rbx
	popq %rax
	cqo
	idivq %rbx
	pushq %rax
	call print_int
	popq %rbx
	movq 16(%rbp), %rax
	pushq %rax
	movq $10, %rbx
	popq %rax
	cqo
	idivq %rbx
	pushq %rdx
	popq %rax
	movq %rax, -16(%rbp)
	movq -16(%rbp), %rax
	pushq %rax
	movq $48, %rax
	popq %rbx
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
	subq $24, %rsp
fact_entry:
	pushq $1
	popq %rax
	movq %rax, -24(%rbp)
	pushq $2
	popq %rax
	movq %rax, -16(%rbp)
while_cond_3:
	movq -16(%rbp), %rax
	pushq %rax
	movq 16(%rbp), %rax
	pushq %rax
	popq %rbx
	popq %rax
	cmpq %rbx, %rax
	jle while_loop_4
	jmp while_end_5
while_loop_4:
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	pushq %rax
	popq %rbx
	popq %rax
	imulq %rbx, %rax
	pushq %rax
	popq %rax
	movq %rax, -24(%rbp)
	movq -16(%rbp), %rax
	pushq %rax
	movq $1, %rbx
	popq %rax
	addq %rbx, %rax
	pushq %rax
	popq %rax
	movq %rax, -16(%rbp)
	jmp while_cond_3
while_end_5:
	movq -24(%rbp), %rax
	pushq %rax
	popq %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	movq %rbp, %rsp
	popq %rbp
	ret
