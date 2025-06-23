BITS 64

global _start

section .text
extern print_char

_start:
	mov rbp, rsp
	sub rsp, 0
main_entry:
	push 6
	call fact
	pop rbx
	push rax
	call print_int
	pop rbx
	push 10
	call print_char
	pop rbx
	mov rax, 60
	xor rdi, rdi
	syscall
print_int:
	push rbp
	mov rbp, rsp
	sub rsp, 8
print_int_entry:
	push 0
	pop rax
	mov [rbp-8], rax
	mov rax, [rbp+16]
	push rax
	push 10
	pop rbx
	pop rax
	cmp rax, rbx
	jl if_then_0
	jmp if_else_1
if_then_0:
	push 48
	mov rax, [rbp+16]
	push rax
	pop rbx
	pop rax
	add rax, rbx
	push rax
	call print_char
	pop rbx
	jmp if_end_2
if_else_1:
	mov rax, [rbp+16]
	push rax
	push 10
	pop rbx
	pop rax
	cqo
	idiv rbx
	push rax
	call print_int
	pop rbx
	mov rax, [rbp+16]
	push rax
	push 10
	pop rbx
	pop rax
	cqo
	idiv rbx
	push rdx
	pop rax
	mov [rbp-8], rax
	push 48
	mov rax, [rbp-8]
	push rax
	pop rbx
	pop rax
	add rax, rbx
	push rax
	call print_char
	pop rbx
	jmp if_end_2
if_end_2:
	mov rsp, rbp
	pop rbp
	ret
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
	jle if_then_3
	jmp if_end_4
if_then_3:
	push 1
	pop rax
	mov rsp, rbp
	pop rbp
	ret
	jmp if_end_4
if_end_4:
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
	pop rbx
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
