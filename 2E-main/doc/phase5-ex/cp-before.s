.data
strFormat:
    .string "%s"
    .align  16
intFormat:
    .string "%d"
    .align  16
.text
.globl main
main:
# initialization of globals (only called in the beginning)
.main:
    # prologue ritual
    pushq   %rbp
    movq    %rsp, %rbp
    # allocate temp space
    addq    $-16, %rsp
.basicBlock2:
    # init a: stack[-8]
    movq    $0, -8(%rbp)
    # init b: stack[-16]
    movq    $0, -16(%rbp)
    # a: stack[-8] = 0
    movq    $0, %rax
    movq    %rax, -8(%rbp)
    # b: stack[-16] = a: stack[-8]
    movq    -8(%rbp), %rax
    movq    %rax, -16(%rbp)
    # epilogue ritual
    leave   
    movq    $0, %rax
    ret     
.failedIndex:
    # set %rax to our desired syscall (1 for exit)
    movq    $1, %rax
    # set %rbx to our desired syscall's argument (1 for nonzero)
    movq    $-1, %rbx
    # interrupt, deferring control to the OS
    int     $0x80
.failedReturn:
    # set %rax to our desired syscall (1 for exit)
    movq    $1, %rax
    # set %rbx to our desired syscall's argument (1 for nonzero)
    movq    $-2, %rbx
    # interrupt, deferring control to the OS
    int     $0x80