.intel_syntax noprefix

.text
.global ultra_entrypoint
	.type ultra_entrypoint, @function
ultra_entrypoint:
	mov rax, 492
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, 1284
	add rax, QWORD PTR [rsp]
	add rsp, 8
	ret


