BITS 64

section .bss
    char_buffer resb 1  ; reserve 1 byte for the character

section .text
    global print_char

print_char:
    ; Input: character to print is passed in stack
    ; Output: None, prints to stdout

    ; pop the character from the stack
    mov rax, [rsp + 8]  ; get the character from the stack (the first argument)

    ; Store the character in char_buffer
    mov [char_buffer], al  ; move the lower byte of rax (the character) into char_buffer

    ; Write the character to stdout
    mov rax, 1          ; syscall number for sys_write
    mov rdi, 1          ; file descriptor 1 (stdout)
    lea rsi, char_buffer  ; load address of char_buffer
    mov rdx, 1          ; number of bytes to write
    syscall

    ret
