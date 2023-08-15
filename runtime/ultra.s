.intel_syntax noprefix

.text
.global ultra_entrypoint
	.type ultra_entrypoint, @function
ultra_entrypoint:
	push rbp
	mov rbp, rsp
	mov rax, 1284
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, 492
	add rax, QWORD PTR [rsp]
	add rsp, 8
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, 936
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, 1728
	sub rax, QWORD PTR [rsp]
	add rsp, 8
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, QWORD PTR [rbp-16]
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, QWORD PTR [rbp-8]
	cmp rax, QWORD PTR [rsp]
	setg al
	sal rax, 7
	or rax, 31
	add rsp, 8
	add rsp, 8
	add rsp, 8
	pop rbp
	ret


