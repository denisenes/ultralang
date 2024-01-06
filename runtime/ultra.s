.intel_syntax noprefix

.extern ULTRA_runtime_error
.text
.global generate0
.type generate0, @function
generate0:
	push rbp
	mov rbp, rsp
	mov rax, rdi
	test rax, rax
	mov rax, 0
	sete al
	movzx rax, al
	sal rax, 7
	or rax, 31
	cmp rax, 31
	je L2
	mov rax, rsi
	jmp L3
L2:
	mov rax, 159
	cmp rax, 31
	je L0
	push rsi
	push rdi
	push rsi
	push rdi
	mov rax, rsi
	mov rsi, rax
	mov rax, rdi
	mov rdi, rax
	call ULTRA_cons
	pop rdi
	pop rsi
	mov rsi, rax
	mov rax, rdi
	sub rax, 4
	mov rdi, rax
	call generate0
	pop rdi
	pop rsi
	jmp L1
L0:
	push rsi
	push rdi
	mov rax, 0
	mov rdi, rax
	call ULTRA_runtime_error
	pop rdi
	pop rsi
L1:
L3:
	pop rbp
	ret

.global generate
.type generate, @function
generate:
	push rbp
	mov rbp, rsp
	mov rax, 40
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, 47
	mov rsi, rax
	mov rax, QWORD PTR [rbp-8]
	mov rdi, rax
	call generate0
	add rsp, 8
	pop rbp
	ret

.global test
.type test, @function
test:
	push rbp
	mov rbp, rsp
	call generate
	pop rbp
	ret

.global ultra_entrypoint
.type ultra_entrypoint, @function
ultra_entrypoint:
	push rbp
	mov rbp, rsp
	call test
	pop rbp
	ret



