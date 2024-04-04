	.text
	.file	"ctypes"
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$1043, %edi
	callq	std_char$putchar@PLT
	movl	$10, %edi
	callq	std_char$putchar@PLT
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc

	.globl	std_char$putchar
	.p2align	4, 0x90
	.type	std_char$putchar,@function
std_char$putchar:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	testl	%edi, %edi
	js	.LBB1_5
	cmpl	$126, %edi
	jle	.LBB1_4
	cmpl	$2047, %edi
	jg	.LBB1_5
	movl	%edi, %eax
	shrl	$6, %eax
	orl	$192, %eax
	andl	$63, %edi
	orl	$128, %edi
	movl	%edi, %ebx
	movl	%eax, %edi
	callq	putchar@PLT
	movl	%ebx, %edi
.LBB1_4:
	callq	putchar@PLT
.LBB1_5:
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	std_char$putchar, .Lfunc_end1-std_char$putchar
	.cfi_endproc

	.section	".note.GNU-stack","",@progbits
