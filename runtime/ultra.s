.intel_syntax noprefix

.extern ULTRA_runtime_error
.text
.global ultra_entrypoint
.type ultra_entrypoint, @function
ultra_entrypoint:
	push rbp
	mov rbp, rsp
	mov rax, 1284
	mov rsi, rax
	mov rax, 492
	mov rdi, rax
	call ULTRA_cons
	pop rbp
	ret



