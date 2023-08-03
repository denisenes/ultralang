.intel_syntax noprefix

.text
.p2align 4,,15
.globl ultra_entrypoint
	.type ultra_entrypoint, @function
ultra_entrypoint:
mov rax, 42
mov rax, 123
ret


