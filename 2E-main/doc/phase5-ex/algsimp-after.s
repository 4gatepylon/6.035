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
    addq    $-96, %rsp
.basicBlock11:
    # init a: stack[-8]
    movq    $0, -8(%rbp)
    # init b: stack[-16]
    movq    $0, -16(%rbp)
    # init c: stack[-24]
    movq    $0, -24(%rbp)
    # b: stack[-16] = 1
    movq    $1, %rax
    movq    %rax, -16(%rbp)
    # a: stack[-8] = 3
    movq    $3, %rax
    movq    %rax, -8(%rbp)
    # t3: stack[-32] = a: stack[-8]
    movq    -8(%rbp), %rax
    movq    %rax, -32(%rbp)
    # a: stack[-8] = b: stack[-16] << 4
    movq    -16(%rbp), %rax
    salq    $4, %rax
    movq    %rax, -8(%rbp)
    # t4: stack[-40] = a: stack[-8]
    movq    -8(%rbp), %rax
    movq    %rax, -40(%rbp)
    # c: stack[-24] = false
    movq    $0, %rax
    movq    %rax, -24(%rbp)
    # t5: stack[-48] = c: stack[-24]
    movq    -24(%rbp), %rax
    movq    %rax, -48(%rbp)
    # a: stack[-8] = b: stack[-16]
    movq    -16(%rbp), %rax
    movq    %rax, -8(%rbp)
    # t6: stack[-56] = a: stack[-8]
    movq    -8(%rbp), %rax
    movq    %rax, -56(%rbp)
    # a: stack[-8] = b: stack[-16]
    movq    -16(%rbp), %rax
    movq    %rax, -8(%rbp)
    # t7: stack[-64] = a: stack[-8]
    movq    -8(%rbp), %rax
    movq    %rax, -64(%rbp)
    # a: stack[-8] = b: stack[-16]
    movq    -16(%rbp), %rax
    movq    %rax, -8(%rbp)
    # a: stack[-8] = b: stack[-16]
    movq    -16(%rbp), %rax
    movq    %rax, -8(%rbp)
    # t8: stack[-72] = a: stack[-8]
    movq    -8(%rbp), %rax
    movq    %rax, -72(%rbp)
    # c: stack[-24] = true
    movq    $1, %rax
    movq    %rax, -24(%rbp)
    # t9: stack[-80] = c: stack[-24]
    movq    -24(%rbp), %rax
    movq    %rax, -80(%rbp)
    # c: stack[-24] = false
    movq    $0, %rax
    movq    %rax, -24(%rbp)
    # t10: stack[-88] = c: stack[-24]
    movq    -24(%rbp), %rax
    movq    %rax, -88(%rbp)
    # a: stack[-8] = - b: stack[-16]
    movq    -16(%rbp), %rax
    neg     %rax
    movq    %rax, -8(%rbp)
    # t11: stack[-96] = a: stack[-8]
    movq    -8(%rbp), %rax
    movq    %rax, -96(%rbp)
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