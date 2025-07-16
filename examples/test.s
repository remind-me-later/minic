.section .data
my_data_start:

.section .text
.globl _start

.extern print_char
.type print_char, @function
.extern print
.type print, @function
_start:
	movq %rsp, %rbp
	subq $24, %rsp
	movq $10, %r8
	movq $10, %rax
	movq %rax, -24(%rbp)
	movq $0, %r8
	movq $0, %rax
	movq %rax, -16(%rbp)
	movq $5, %r8
	pushq $5
	call fact
	addq $8, %rsp
	movq %rax, %r8
	movq %r8, -8(%rbp)
	movq $20, %r8
	movq $20, %rax
	movq %rax, -16(%rbp)
	movq -24(%rbp), %r8
	pushq %r8
	call print_int
	addq $8, %rsp
	movq $32, %r8
	pushq $32
	call print
	addq $8, %rsp
	movq -16(%rbp), %r8
	pushq %r8
	call print_int
	addq $8, %rsp
	movq $32, %r8
	pushq $32
	call print
	addq $8, %rsp
	movq -8(%rbp), %r8
	pushq %r8
	call print_int
	addq $8, %rsp
	movq $10, %r8
	pushq $10
	call print
	addq $8, %rsp
	movq $60, %rax
	xorq %rdi, %rdi
	syscall
print_int:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	movq $0, %r8
	movq $0, %rax
	movq %rax, -8(%rbp)
	movq 16(%rbp), %r8
	movq %r8, %rax
	cmpq $10, %rax
	movq %rax, %r8
	jl IL0
	jmp IL1
IL0:
	movq 16(%rbp), %r8
	movq $48, %rax
	addq %r8, %rax
	movq %rax, %r8
	pushq %r8
	call print_char
	addq $8, %rsp
	jmp IL2
IL1:
	movq 16(%rbp), %r8
	movq %r8, %rax
	cqo
	movq $10, %rbx
	idivq %rbx
	movq %rax, %r8
	pushq %r8
	call print_int
	addq $8, %rsp
	movq 16(%rbp), %r8
	movq %r8, %rax
	cqo
	movq $10, %rbx
	idivq %rbx
	movq %rdx, %rax
	movq %rax, %r8
	movq %r8, -8(%rbp)
	movq -8(%rbp), %r8
	movq $48, %rax
	addq %r8, %rax
	movq %rax, %r8
	pushq %r8
	call print_char
	addq $8, %rsp
	jmp IL2
IL2:
	movq %rbp, %rsp
	popq %rbp
	ret
fact:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq $1, %r9
	movq $1, %rax
	movq %rax, -8(%rbp)
	movq $2, %r9
	movq $2, %rax
	movq %rax, -16(%rbp)
	movq $2, %r9
	movq $2, %rax
	movq %rax, -16(%rbp)
	jmp FL3
FL3:
	movq -16(%rbp), %r9
	movq 16(%rbp), %r8
	movq %r9, %rax
	cmpq %r8, %rax
	movq %rax, %r8
	jle FL4
	jmp FL5
FL4:
	movq -8(%rbp), %r8
	movq -16(%rbp), %r9
	movq %r8, %rax
	imulq %r9, %rax
	movq %rax, %r9
	movq %r9, -8(%rbp)
	movq -16(%rbp), %r9
	movq %r9, %rax
	addq $1, %rax
	movq %rax, %r9
	movq %r9, -16(%rbp)
	jmp FL3
FL5:
	movq -8(%rbp), %r9
	movq %r9, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
