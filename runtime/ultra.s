.intel_syntax noprefix

.text
.global ultra_entrypoint
	.type ultra_entrypoint, @function
ultra_entrypoint:
	mov rax, 20
	sub rsp, 8
	mov QWORD PTR [rsp], rax
	mov rax, -492
	sar QWORD PTR [rsp], 2
	imul rax, QWORD PTR [rsp]
	add rsp, 8
	ret


