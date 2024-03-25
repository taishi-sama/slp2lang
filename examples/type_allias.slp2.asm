	.text
	.file	"type_allias"
	.globl	type_allias$identity
	.p2align	4, 0x90
	.type	type_allias$identity,@function
type_allias$identity:
	.cfi_startproc
	movl	%edi, %eax
	retq
.Lfunc_end0:
	.size	type_allias$identity, .Lfunc_end0-type_allias$identity
	.cfi_endproc

	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$10, %edi
	callq	type_allias$identity@PLT
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc

	.section	".note.GNU-stack","",@progbits
