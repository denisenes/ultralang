.intel_syntax noprefix

.text
.global ultra_entrypoint
	.type ultra_entrypoint, @function
ultra_entrypoint:
	push rbp
	mov rbp, rsp
	mov rax, 4000
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, 4000000
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, QWORD PTR [rbp-16]
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, QWORD PTR [rbp-8]
	cmp rax, QWORD PTR [rsp]
	setg al
	movzx rax, al
	sal rax, 7
	or rax, 31
	add rsp, 8
	cmp rax, 31
	je L0
	mov rax, 159
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, QWORD PTR [rbp-24]
	add rsp, 8
	jmp L1
L0:
	mov rax, 31
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, QWORD PTR [rbp-24]
	add rsp, 8
L1:
	add rsp, 8
	add rsp, 8
	pop rbp
	ret


