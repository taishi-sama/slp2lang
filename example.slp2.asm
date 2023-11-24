	.text
	.file	"example.slp2"
	.globl	test
	.p2align	4, 0x90
	.type	test,@function
test:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$.L__unnamed_1, %edi
	callq	puts@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
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
	movl	$.L__unnamed_2, %edi
	callq	puts@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc

	.type	.L__unnamed_1,@object
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__unnamed_1:
	.asciz	"Hello!"
	.size	.L__unnamed_1, 7

	.type	.L__unnamed_2,@object
.L__unnamed_2:
	.asciz	"HIIIII"
	.size	.L__unnamed_2, 7

	.section	".note.GNU-stack","",@progbits
