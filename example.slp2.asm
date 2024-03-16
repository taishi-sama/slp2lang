	.text
	.file	"example.slp2"
	.globl	example$writechar
	.p2align	4, 0x90
	.type	example$writechar,@function
example$writechar:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movzbl	%dil, %edi
	callq	putchar@PLT
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	example$writechar, .Lfunc_end0-example$writechar
	.cfi_endproc

	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$72, %edi
	callq	example$writechar@PLT
	movl	$101, %edi
	callq	example$writechar@PLT
	movl	$108, %edi
	callq	example$writechar@PLT
	movl	$108, %edi
	callq	example$writechar@PLT
	movl	$111, %edi
	callq	example$writechar@PLT
	movl	$32, %edi
	callq	example$writechar@PLT
	movl	$119, %edi
	callq	example$writechar@PLT
	movl	$111, %edi
	callq	example$writechar@PLT
	movl	$114, %edi
	callq	example$writechar@PLT
	movl	$108, %edi
	callq	example$writechar@PLT
	movl	$100, %edi
	callq	example$writechar@PLT
	movl	$33, %edi
	callq	example$writechar@PLT
	movl	$10, %edi
	callq	example$writechar@PLT
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc

	.section	".note.GNU-stack","",@progbits
