.section .bss
    .lcomm char_buffer, 1

.section .text
    .globl print_char

print_char:
    # Input: character to print is passed in stack
    # Output: None, prints to stdout

    # pop the character from the stack
    movq 8(%rsp), %rax  # move the character from the stack into rax

    # Store the character in char_buffer
    movb %al, char_buffer  
    

    # Write the character to stdout
    movq $1, %rax                   # syscall number for sys_write
    movq $1, %rdi                   # file descriptor 1 (stdout)
    lea char_buffer(%rip), %rsi     # load address of char_buffer into rsi
    movq $1, %rdx                   # number of bytes to write
    syscall

    ret
