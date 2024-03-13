	.text
	.file	"example.slp2.o"
	.globl	test
	.p2align	4, 0x90
	.type	test,@function
test:
	.cfi_startproc
	retq
.Lfunc_end0:
	.size	test, .Lfunc_end0-test
	.cfi_endproc

	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$72, %edi
	callq	putchar@PLT
	movl	$101, %edi
	callq	putchar@PLT
	movl	$108, %edi
	callq	putchar@PLT
	movl	$108, %edi
	callq	putchar@PLT
	movl	$111, %edi
	callq	putchar@PLT
	movl	$32, %edi
	callq	putchar@PLT
	movl	$119, %edi
	callq	putchar@PLT
	movl	$111, %edi
	callq	putchar@PLT
	movl	$114, %edi
	callq	putchar@PLT
	movl	$108, %edi
	callq	putchar@PLT
	movl	$100, %edi
	callq	putchar@PLT
	movl	$33, %edi
	callq	putchar@PLT
	movl	$10, %edi
	callq	putchar@PLT
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc

	.section	".note.GNU-stack","",@progbits
