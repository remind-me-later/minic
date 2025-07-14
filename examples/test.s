.section .text
.globl _start

.extern print
.type print, @function
_start:
	movq %rsp, %rbp
	subq $18, %rsp
	movq $65, %r8
	movq $65, %rax
	movq %rax, -10(%rbp)
	movq -10(%rbp), %r8
	movq %r8, -18(%rbp)
	movq -18(%rbp), %r8
	movq %r8, %rax
	leaq (%rax), %rax
	movq %rax, %r8
	pushq %r8
	call print
	addq $8, %rsp
	movq $10, %r8
	pushq $10
	call print
	addq $8, %rsp
	movq $60, %rax
	xorq %rdi, %rdi
	syscall
