BITS 64

global _start

section .text
extern print_char

_start:
	sub rsp, 0
main_entry:
	push 4
	call fib
	push 4
	pop rbx
	pop rax
	mul rax, rbx
	push rax
	call print_char
	mov 60, rax
	xor rdi, rdi
	syscall
fib:
	push rbp
	mov rbp, rsp
	sub rsp, 0
fib_entry:
	mov rax, [rbp+16]
	push rax
	push 1
	pop rbx
	pop rax
	cmp rax, rbx
	jle if_then_0
	jmp if_end_1
if_then_0:
	push 1
	mov rsp, rbp
	pop rbp
	ret
	jmp if_end_1
if_end_1:
	mov rax, [rbp+16]
	push rax
	mov rax, [rbp+16]
	push rax
	push 1
	pop rbx
	pop rax
	sub rax, rbx
	push rax
	call fib
	pop rbx
	pop rax
	mul rax, rbx
	push rax
	mov rsp, rbp
	pop rbp
	ret
	mov rsp, rbp
	pop rbp
	ret

