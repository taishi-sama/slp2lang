	.text
	.file	"std"
	.globl	std$identity
	.p2align	4, 0x90
	.type	std$identity,@function
std$identity:
	.cfi_startproc
	movl	%edi, %eax
	retq
.Lfunc_end0:
	.size	std$identity, .Lfunc_end0-std$identity
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

	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$72, %edi
	callq	std_io$writechar@PLT
	movl	$101, %edi
	callq	std_io$writechar@PLT
	movl	$108, %edi
	callq	std_io$writechar@PLT
	movl	$108, %edi
	callq	std_io$writechar@PLT
	movl	$111, %edi
	callq	std_io$writechar@PLT
	movl	$32, %edi
	callq	std_io$writechar@PLT
	movl	$119, %edi
	callq	std_io$writechar@PLT
	movl	$111, %edi
	callq	std_io$writechar@PLT
	movl	$114, %edi
	callq	std_io$writechar@PLT
	movl	$108, %edi
	callq	std_io$writechar@PLT
	movl	$100, %edi
	callq	std_io$writechar@PLT
	movl	$33, %edi
	callq	std_io$writechar@PLT
	movl	$10, %edi
	callq	std_io$writechar@PLT
	callq	std_io$getchar@PLT
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end3:
	.size	main, .Lfunc_end3-main
	.cfi_endproc

	.section	".note.GNU-stack","",@progbits
