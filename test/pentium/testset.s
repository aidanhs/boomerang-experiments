	.file	"hello.c"
	.section	.rodata
.LC0:
	.string	"Hello, set\n"
.LC_cs:
    .string "Carry was set\n";
.LC_cns:
    .string "Carry was NOT set\n";
table_c:
    .long   .LC_cs, .LC_cns
.LC_nns:
    .string "Negative was NOT set\n";
.LC_ns:
    .string "Negative was set\n";
table_n:
    .long   .LC_nns, .LC_ns

	.text
.globl main
	.type	main,@function
main:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	andl	$-16, %esp
	movl	$0, %eax
	subl	%eax, %esp
	subl	$12, %esp
	pushl	$.LC0
	call	printf

# Tests

    mov     8(%ebp), %eax    # argc
    mov     $0, %edx
    cmpl    $3, %eax
    setc    %dl
    movl    table_c(,%edx,4), %ecx
    push    %ecx
    call    printf

    mov     8(%ebp), %eax    # argc
    mov     $0, %ecx
    subl    $3, %eax
    setns   %cl
    movl    table_n(,%ecx,4), %eax
    push    %eax
    call    printf

    xorl    %eax, %eax
	addl	$16, %esp
	leave
	ret
.Lfe1:
	.size	main,.Lfe1-main
	.ident	"GCC: (GNU) 3.2.2 20030222 (Red Hat Linux 3.2.2-5)"
