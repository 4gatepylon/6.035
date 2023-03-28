.data
strFormat:
    .string "%s"
    .align  16
intFormat:
    .string "%d"
    .align  16
.text
.globl main
get_int:
    # prologue ritual
    pushq   %rbp
    movq    %rsp, %rbp
    # allocate temp space
    addq    $0, %rsp
.basicBlock1:
    # return %rdi
    movq    %rdi, %rax
    leave   
    ret     
    jmp     .failedReturn
main:
# initialization of globals (only called in the beginning)
.main:
    # prologue ritual
    pushq   %rbp
    movq    %rsp, %rbp
    # allocate temp space
    addq    $-32, %rsp
.basicBlock6:
    # init a: stack[-8]
    movq    $0, -8(%rbp)
    # init b: stack[-16]
    movq    $0, -16(%rbp)
    # a: stack[-8] = 0
    movq    $0, %rax
    movq    %rax, -8(%rbp)
    # b: stack[-16] = 1
    movq    $1, %rax
    movq    %rax, -16(%rbp)
    # t3: stack[-24] = call get_int(b: stack[-16])
    pushq   %rdi
    pushq   %rsi
    pushq   %rdx
    pushq   %rcx
    pushq   %r8
    pushq   %r9
    xor     %rax, %rax
    movq    -16(%rbp), %rdi
    call    get_int
    movq    %rax, -24(%rbp)
    popq    %r9
    popq    %r8
    popq    %rcx
    popq    %rdx
    popq    %rsi
    popq    %rdi
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