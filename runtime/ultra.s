.intel_syntax noprefix

.text
.global ultra_entrypoint
	.type ultra_entrypoint, @function
ultra_entrypoint:
	mov rax, 159
	cmp rax, 47
	sete al
	sal rax, 7
	or rax, 31
	ret


