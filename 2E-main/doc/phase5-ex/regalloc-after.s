.data
strFormat:
    .string "%s"
    .align  16
intFormat:
    .string "%d"
    .align  16
.text
.globl main
func:
    # prologue ritual
    pushq   %rbp
    movq    %rsp, %rbp
    pushq   %r12
    pushq   %r13
    pushq   %r14
    pushq   %r15
    pushq   %rbx
    # allocate temp space
    addq    $0, %rsp
.basicBlock1:
    # 0: return %rdi
    movq    %rdi, %rax
    subq    $0, %rsp
    popq    %rbx
    popq    %r15
    popq    %r14
    popq    %r13
    popq    %r12
    leave   
    ret     
    jmp     .failedReturn
main:
# initialization of globals (only called in the beginning)
.main:
    # prologue ritual
    pushq   %rbp
    movq    %rsp, %rbp
    pushq   %r12
    pushq   %r13
    pushq   %r14
    pushq   %r15
    pushq   %rbx
    # allocate temp space
    addq    $-96, %rsp
.basicBlock15:
    # 0: init a: stack[-8]
    movq    $0, -8(%rbp)
    # 1: init b: stack[-16]
    movq    $0, -16(%rbp)
    # 2: init c: stack[-24]
    movq    $0, -24(%rbp)
    # 3: init d: stack[-32]
    movq    $0, -32(%rbp)
    # 4: init e: stack[-40]
    movq    $0, -40(%rbp)
    # 5: init f: stack[-48]
    movq    $0, -48(%rbp)
    # 6: a: stack[-8] = 0
    movq    $0, %rax
    movq    %rax, %r15
    # 7: e: stack[-40] = 0
    movq    $0, %rax
    movq    %rax, %r12
    # 8: c: stack[-24] = 0
    movq    $0, %rax
    movq    %rax, %r13
    # 9: d: stack[-32] = 0
    movq    $0, %rax
    movq    %rax, %r14
    # 10: t7: stack[-56] = call func(c: stack[-24])
    pushq   %rdi
    pushq   %rsi
    pushq   %rdx
    pushq   %rcx
    pushq   %r8
    pushq   %r9
    xor     %rax, %rax
    movq    %r13, %rdi
    call    func
    movq    %rax, -56(%rbp)
    popq    %r9
    popq    %r8
    popq    %rcx
    popq    %rdx
    popq    %rsi
    popq    %rdi
    # 11: t8: stack[-64] = call func(d: stack[-32])
    pushq   %rdi
    pushq   %rsi
    pushq   %rdx
    pushq   %rcx
    pushq   %r8
    pushq   %r9
    xor     %rax, %rax
    movq    %r14, %rdi
    call    func
    movq    %rax, -64(%rbp)
    popq    %r9
    popq    %r8
    popq    %rcx
    popq    %rdx
    popq    %rsi
    popq    %rdi
    # 12: b: stack[-16] = 0
    movq    $0, %rax
    movq    %rax, %r13
    # 13: t9: stack[-72] = call func(a: stack[-8])
    pushq   %rdi
    pushq   %rsi
    pushq   %rdx
    pushq   %rcx
    pushq   %r8
    pushq   %r9
    xor     %rax, %rax
    movq    %r15, %rdi
    call    func
    movq    %rax, -72(%rbp)
    popq    %r9
    popq    %r8
    popq    %rcx
    popq    %rdx
    popq    %rsi
    popq    %rdi
    # 14: t10: stack[-80] = call func(b: stack[-16])
    pushq   %rdi
    pushq   %rsi
    pushq   %rdx
    pushq   %rcx
    pushq   %r8
    pushq   %r9
    xor     %rax, %rax
    movq    %r13, %rdi
    call    func
    movq    %rax, -80(%rbp)
    popq    %r9
    popq    %r8
    popq    %rcx
    popq    %rdx
    popq    %rsi
    popq    %rdi
    # 15: f: stack[-48] = 0
    movq    $0, %rax
    movq    %rax, %r13
    # 16: t11: stack[-88] = call func(e: stack[-40])
    pushq   %rdi
    pushq   %rsi
    pushq   %rdx
    pushq   %rcx
    pushq   %r8
    pushq   %r9
    xor     %rax, %rax
    movq    %r12, %rdi
    call    func
    movq    %rax, -88(%rbp)
    popq    %r9
    popq    %r8
    popq    %rcx
    popq    %rdx
    popq    %rsi
    popq    %rdi
    # 17: t12: stack[-96] = call func(f: stack[-48])
    pushq   %rdi
    pushq   %rsi
    pushq   %rdx
    pushq   %rcx
    pushq   %r8
    pushq   %r9
    xor     %rax, %rax
    movq    %r13, %rdi
    call    func
    movq    %rax, -96(%rbp)
    popq    %r9
    popq    %r8
    popq    %rcx
    popq    %rdx
    popq    %rsi
    popq    %rdi
    # epilogue ritual
    subq    $-96, %rsp
    popq    %rbx
    popq    %r15
    popq    %r14
    popq    %r13
    popq    %r12
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