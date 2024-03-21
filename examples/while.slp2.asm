	.text
	.file	"cstd"
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movb	$65, %bl
	cmpb	$119, %bl
	jg	.LBB0_3
	.p2align	4, 0x90
.LBB0_2:
	movzbl	%bl, %edi
	callq	std_io$writechar@PLT
	incb	%bl
	cmpb	$119, %bl
	jle	.LBB0_2
.LBB0_3:
	movl	$10, %edi
	callq	std_io$writechar@PLT
	xorl	%eax, %eax
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
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
.Lfunc_end2:
	.size	std_io$getchar, .Lfunc_end2-std_io$getchar
	.cfi_endproc

	.section	".note.GNU-stack","",@progbits
