.section .data
my_data_start:
.global a
.type a, @object
a:
.zero 1

.section .text
.globl _start

.extern print
.type print, @function
_start:
	movq %rsp, %rbp
	subq $8, %rsp
	movq $65, %r8
	movq $65, my_data_start+0(%rip)
	movq my_data_start+0(%rip), %r8
	movq %r8, -8(%rbp)
	movq -8(%rbp), %r8
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
