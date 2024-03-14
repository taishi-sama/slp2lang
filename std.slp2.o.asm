	.text
	.file	"std.slp2.o"
	.globl	identity
	.p2align	4, 0x90
	.type	identity,@function
identity:
	.cfi_startproc
	movl	%edi, %eax
	retq
.Lfunc_end0:
	.size	identity, .Lfunc_end0-identity
	.cfi_endproc

	.section	".note.GNU-stack","",@progbits
