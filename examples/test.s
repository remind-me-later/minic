.section .text
.globl _start

.extern print_char
.type print_char, @function

_start:
	movq %rsp, %rbp
	subq $24, %rsp
main_entry:
	pushq $0
	pushq $-8
	popq %rbx
	popq %rax
	imulq %rbx, %rax
	pushq %rax
	pushq $10
	popq %rax
	popq %rsi
	movq %rax, 0(%rbp, %rsi)
	pushq $1
	pushq $-8
	popq %rbx
	popq %rax
	imulq %rbx, %rax
	pushq %rax
	pushq $0
	popq %rax
	popq %rsi
	movq %rax, 0(%rbp, %rsi)
	pushq $2
	pushq $-8
	popq %rbx
	popq %rax
	imulq %rbx, %rax
	pushq %rax
	pushq $0
	popq %rax
	popq %rsi
	movq %rax, 0(%rbp, %rsi)
	pushq $1
	pushq $-8
	popq %rbx
	popq %rax
	imulq %rbx, %rax
	pushq %rax
	pushq $20
	popq %rax
	popq %rsi
	movq %rax, 0(%rbp, %rsi)
	pushq $0
	pushq $-8
	popq %rbx
	popq %rax
	imulq %rbx, %rax
	pushq %rax
	popq %rsi
	movq 0(%rbp, %rsi), %rax
	pushq %rax
	call print_int
	popq %rbx
	pushq $32
	call print_char
	popq %rbx
	pushq $1
	pushq $-8
	popq %rbx
	popq %rax
	imulq %rbx, %rax
	pushq %rax
	popq %rsi
	movq 0(%rbp, %rsi), %rax
	pushq %rax
	call print_int
	popq %rbx
	pushq $32
	call print_char
	popq %rbx
	pushq $2
	pushq $-8
	popq %rbx
	popq %rax
	imulq %rbx, %rax
	pushq %rax
	popq %rsi
	movq 0(%rbp, %rsi), %rax
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
	movq %rax, -16(%rbp)
	pushq $48
	movq -16(%rbp), %rax
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
