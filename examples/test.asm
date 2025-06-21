global _start
section .text
extern print_char

_start:
sub rsp, 0
entry_0:
mov rax, 72
push rax
call print_char

mov rax, 60
xor rdi, rdi
syscall

