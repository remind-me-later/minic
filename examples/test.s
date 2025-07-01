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
	movq %r8, %rax
	movq %rax, (%rbp)
	movq $0, %r8
	movq %r8, %rax
	movq %rax, -8(%rbp)
	movq $5, %r8
	pushq %r8
	call fact
	popq %rbx
	movq %rax, %r8
	movq %r8, %rax
	movq %rax, -16(%rbp)
	movq $20, %r8
	movq %r8, %rax
	movq %rax, -8(%rbp)
	movq (%rbp), %rax
	movq %rax, %r8
	pushq %r8
	call print_int
	popq %rbx
	movq $32, %r8
	pushq %r8
	call print
	popq %rbx
	movq -8(%rbp), %rax
	movq %rax, %r8
	pushq %r8
	call print_int
	popq %rbx
	movq $32, %r8
	pushq %r8
	call print
	popq %rbx
	movq -16(%rbp), %rax
	movq %rax, %r8
	pushq %r8
	call print_int
	popq %rbx
	movq $10, %r8
	pushq %r8
	call print
	popq %rbx
	movq $60, %rax
	xorq %rdi, %rdi
	syscall
print_int:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq $0, %r8
	movq %r8, %rax
	movq %rax, -16(%rbp)
	movq 16(%rbp), %rax
	movq %rax, %r8
	movq %r8, %rax
	cmpq $10, %rax
	movq %rax, %r8
	jl IL0
	jmp IL1
IL0:
	movq 16(%rbp), %rax
	movq %rax, %r8
	movq $48, %rax
	addq %r8, %rax
	movq %rax, %r8
	pushq %r8
	call print_char
	popq %rbx
	jmp IL2
IL1:
	movq 16(%rbp), %rax
	movq %rax, %r8
	movq %r8, %rax
	cqo
	movq $10, %rbx
	idivq %rbx
	movq %rax, %r8
	pushq %r8
	call print_int
	popq %rbx
	movq 16(%rbp), %rax
	movq %rax, %r8
	movq %r8, %rax
	cqo
	movq $10, %rbx
	idivq %rbx
	movq %rdx, %rax
	movq %rax, %r8
	movq %r8, %rax
	movq %rax, -16(%rbp)
	movq -16(%rbp), %rax
	movq %rax, %r8
	movq $48, %rax
	addq %r8, %rax
	movq %rax, %r8
	pushq %r8
	call print_char
	popq %rbx
	jmp IL2
IL2:
	movq %rbp, %rsp
	popq %rbp
	ret
fact:
	pushq %rbp
	movq %rsp, %rbp
	subq $24, %rsp
	movq $1, %r9
	movq %r9, %rax
	movq %rax, -24(%rbp)
	movq $2, %r9
	movq %r9, %rax
	movq %rax, -16(%rbp)
	jmp WL3
WL3:
	movq -16(%rbp), %rax
	movq %rax, %r9
	movq 16(%rbp), %rax
	movq %rax, %r8
	movq %r9, %rax
	cmpq %r8, %rax
	movq %rax, %r8
	jle WL4
	jmp WL5
WL4:
	movq -24(%rbp), %rax
	movq %rax, %r8
	movq -16(%rbp), %rax
	movq %rax, %r9
	movq %r8, %rax
	imulq %r9, %rax
	movq %rax, %r9
	movq %r9, %rax
	movq %rax, -24(%rbp)
	movq -16(%rbp), %rax
	movq %rax, %r9
	movq %r9, %rax
	addq $1, %rax
	movq %rax, %r9
	movq %r9, %rax
	movq %rax, -16(%rbp)
	jmp WL3
WL5:
	movq -24(%rbp), %rax
	movq %rax, %r9
	movq %r9, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
