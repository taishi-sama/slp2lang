	.text
	.file	"std_io"
	.globl	std_io$getchar
	.p2align	4, 0x90
	.type	std_io$getchar,@function
std_io$getchar:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	getchar@PLT
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	std_io$getchar, .Lfunc_end0-std_io$getchar
	.cfi_endproc

	.globl	std_io$writechar
	.p2align	4, 0x90
	.type	std_io$writechar,@function
std_io$writechar:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movzbl	%dil, %edi
	callq	putchar@PLT
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	std_io$writechar, .Lfunc_end1-std_io$writechar
	.cfi_endproc

	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	std_io$getchar@PLT
	movl	%eax, %ecx
	xorl	%eax, %eax
	cmpl	$10, %ecx
	jg	.LBB2_2
	movl	$1, %eax
.LBB2_2:
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end2:
	.size	main, .Lfunc_end2-main
	.cfi_endproc

	.section	".note.GNU-stack","",@progbits
