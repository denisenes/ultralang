.intel_syntax noprefix

.text
.global ultra_entrypoint
	.type ultra_entrypoint, @function
ultra_entrypoint:
	mov rax, 4000
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, 493824
	add rax, QWORD PTR [rsp]
	add rsp, 8
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, 493824
	cmp rax, QWORD PTR [rsp]
	setg al
	sal rax, 7
	or rax, 31
	add rsp, 8
	ret


