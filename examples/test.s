.section .text
.globl _start

.extern print_char
.type print_char, @function
.extern print
.type print, @function

_start:
	movq %rsp, %rbp
	subq $1, %rsp
main_entry:
	pushq $65
	popq %rax
	movq %rax, (%rbp)
	movq (%rbp), %rax
	pushq %rax
	call print
	popq %rbx
	pushq $10
	call print
	popq %rbx
	movq $60, %rax
	xorq %rdi, %rdi
	syscall
