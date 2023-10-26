	.text
	.file	"example.slp2"
	.globl	hello
	.p2align	4, 0x90
	.type	hello,@function
hello:
	.cfi_startproc
	retq
.Lfunc_end0:
	.size	hello, .Lfunc_end0-hello
	.cfi_endproc

	.globl	hi
	.p2align	4, 0x90
	.type	hi,@function
hi:
	.cfi_startproc
	retq
.Lfunc_end1:
	.size	hi, .Lfunc_end1-hi
	.cfi_endproc

	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$.L__unnamed_1, %edi
	callq	puts@PLT
	movl	$.L__unnamed_2, %edi
	callq	puts@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end2:
	.size	main, .Lfunc_end2-main
	.cfi_endproc

	.type	.L__unnamed_1,@object
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__unnamed_1:
	.asciz	"Hello world!"
	.size	.L__unnamed_1, 13

	.type	.L__unnamed_2,@object
.L__unnamed_2:
	.asciz	"HIIIII"
	.size	.L__unnamed_2, 7

	.section	".note.GNU-stack","",@progbits
