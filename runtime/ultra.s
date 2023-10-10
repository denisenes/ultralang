.intel_syntax noprefix

.extern ULTRA_runtime_error
.text
.global fibonacci
.type fibonacci, @function
fibonacci:
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
	je L4
	mov rax, 4
	jmp L5
L4:
	mov rax, 4
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
	je L2
	mov rax, 4
	jmp L3
L2:
	mov rax, 4
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, rdi
	cmp rax, QWORD PTR [rsp]
	setg al
	movzx rax, al
	sal rax, 7
	or rax, 31
	add rsp, 8
	cmp rax, 31
	je L0
	push rdi
	mov rax, 8
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, rdi
	sub rax, QWORD PTR [rsp]
	add rsp, 8
	mov rdi, rax
	call fibonacci
	pop rdi
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	push rdi
	mov rax, 4
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, rdi
	sub rax, QWORD PTR [rsp]
	add rsp, 8
	mov rdi, rax
	call fibonacci
	pop rdi
	add rax, QWORD PTR [rsp]
	add rsp, 8
	jmp L1
L0:
	push rdi
	mov rax, 4
	mov rdi, rax
	call ULTRA_runtime_error
	pop rdi
L1:
L3:
L5:
	pop rbp
	ret

.global ultra_entrypoint
.type ultra_entrypoint, @function
ultra_entrypoint:
	push rbp
	mov rbp, rsp
	mov rax, -492
	mov rdi, rax
	call fibonacci
	pop rbp
	ret



