.intel_syntax noprefix

.extern ULTRA_runtime_error
.extern ULTRA_cons
.text
.global ultra_entrypoint
.type ultra_entrypoint, @function
ultra_entrypoint:
	push rbp
	mov rbp, rsp
	call test_map
	pop rbp
	ret

.global test_map
.type test_map, @function
test_map:
	push rbp
	mov rbp, rsp
	mov rax, 47
	mov rsi, rax
	mov rax, 20
	mov rdi, rax
	call ULTRA_cons
	mov rsi, rax
	mov rax, 16
	mov rdi, rax
	call ULTRA_cons
	mov rsi, rax
	mov rax, 12
	mov rdi, rax
	call ULTRA_cons
	mov rsi, rax
	mov rax, 8
	mov rdi, rax
	call ULTRA_cons
	mov rsi, rax
	mov rax, 4
	mov rdi, rax
	call ULTRA_cons
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, QWORD PTR [rbp-8]
	mov rsi, rax
	lea rax, [sqr]
	mov rdi, rax
	call map
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, 0
	mov rsi, rax
	mov rax, QWORD PTR [rbp-16]
	mov rdi, rax
	call sum_list
	add rsp, 8
	add rsp, 8
	pop rbp
	ret

.global sum_list
.type sum_list, @function
sum_list:
	push rbp
	mov rbp, rsp
	mov rax, rdi
	cmp rax, 47
	sete al
	movzx rax, al
	sal rax, 7
	or rax, 31
	cmp rax, 31
	je L6
	mov rax, rsi
	jmp L7
L6:
	mov rax, 159
	cmp rax, 31
	je L4
	push rsi
	push rdi
	mov rax, rdi
	and rax, -8
	mov rax, QWORD PTR [rax+8]
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, rsi
	add rax, QWORD PTR [rsp]
	add rsp, 8
	mov rsi, rax
	mov rax, rdi
	and rax, -8
	mov rax, QWORD PTR [rax+16]
	mov rdi, rax
	call sum_list
	pop rdi
	pop rsi
	jmp L5
L4:
	push rsi
	push rdi
	mov rax, 0
	mov rdi, rax
	call ULTRA_runtime_error
	pop rdi
	pop rsi
L5:
L7:
	pop rbp
	ret

.global sqr
.type sqr, @function
sqr:
	push rbp
	mov rbp, rsp
	mov rax, rdi
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, rdi
	sar QWORD PTR [rsp], 2
	imul rax, QWORD PTR [rsp]
	add rsp, 8
	pop rbp
	ret

.global map
.type map, @function
map:
	push rbp
	mov rbp, rsp
	mov rax, rsi
	cmp rax, 47
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
	mov rax, rsi
	and rax, -8
	mov rax, QWORD PTR [rax+8]
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, rsi
	and rax, -8
	mov rax, QWORD PTR [rax+16]
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	push rsi
	push rdi
	push rsi
	push rdi
	mov rax, QWORD PTR [rbp-16]
	mov rsi, rax
	mov rax, rdi
	mov rdi, rax
	call map
	pop rdi
	pop rsi
	mov rsi, rax
	push rsi
	push rdi
	mov rax, QWORD PTR [rbp-8]
	mov rdi, rax
	mov rax, rdi
	call rax
	pop rdi
	pop rsi
	mov rdi, rax
	call ULTRA_cons
	pop rdi
	pop rsi
	add rsp, 8
	add rsp, 8
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



