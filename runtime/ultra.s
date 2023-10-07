.intel_syntax noprefix

.text
dec:
	push rbp
	mov rbp, rsp
	mov rax, 4
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, rdi
	sub rax, QWORD PTR [rsp]
	add rsp, 8
	pop rbp
	ret

factorial:
	push rbp
	mov rbp, rsp
	mov rax, 0
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, rdi
	cmp rax, QWORD PTR [rsp]
	sete al
	movzx rax, al
	sal rax, 7
	or rax, 31
	add rsp, 8
	cmp rax, 31
	je L0
	mov rax, 4
	jmp L1
L0:
	mov rax, rdi
	sub rax, 4
	mov rdi, rax
	call factorial
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, rdi
	sar QWORD PTR [rsp], 2
	imul rax, QWORD PTR [rsp]
	add rsp, 8
L1:
	pop rbp
	ret

.global ultra_entrypoint
.type ultra_entrypoint, @function
ultra_entrypoint:
	push rbp
	mov rbp, rsp
	mov rax, 40
	mov rdi, rax
	call factorial
	pop rbp
	ret



