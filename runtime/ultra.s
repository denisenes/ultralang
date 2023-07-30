.intel_syntax noprefix

.text
	.p2align 4,,15
	
.globl ultra_entrypoint
	.type ultra_entrypoint, @function
ultra_entrypoint:
	mov eax, 42
	ret