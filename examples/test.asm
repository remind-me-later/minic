BITS 64

global _start

section .text
extern print_char

_start:
	mov rbp, rsp
	sub rsp, 8
main_entry:
	push 2
	call fact
	push rax
	pop rax
	mov [rbp+0], rax
	push 48
	mov rax, [rbp+0]
	push rax
	pop rbx
	pop rax
	add rax, rbx
	push rax
	call print_char
	mov rax, 60
	xor rdi, rdi
	syscall
fact:
	push rbp
	mov rbp, rsp
	sub rsp, 0
fact_entry:
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
	pop rax
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
	call fact
	push rax
	pop rbx
	pop rax
	imul rax, rbx
	push rax
	pop rax
	mov rsp, rbp
	pop rbp
	ret
	mov rsp, rbp
	pop rbp
	ret

