.global {[function_prefix]s}musl_memcpy
// Windows does not support the type directive.
// .type {[function_prefix]s}musl_memcpy,@function
{[function_prefix]s}musl_memcpy:
	push %esi
	push %edi
	mov 12(%esp),%edi
	mov 16(%esp),%esi
	mov 20(%esp),%ecx
	mov %edi,%eax
	cmp $4,%ecx
	jc 1f
	test $3,%edi
	jz 1f
2:	movsb
	dec %ecx
	test $3,%edi
	jnz 2b
1:	mov %ecx,%edx
	shr $2,%ecx
	rep
	movsl
	and $3,%edx
	jz 1f
2:	movsb
	dec %edx
	jnz 2b
1:	pop %edi
	pop %esi
	ret