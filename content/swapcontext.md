context.h

	#ifndef _CONTEXT_H
	#define _CONTEXT_H	
	
	struct Context {
	    uint64_t rsp;
	    uint64_t r15;
	    uint64_t r14;
	    uint64_t r13;
	    uint64_t r12;
	    uint64_t rbx;
	    uint64_t rbp;
	};
	void SwapContext(struct Context *old, struct Context *new);

context.s

	.globl _SwapContext, SwapContext
	_SwapContext:
	SwapContext:
	
	        mov     %rsp, 0x00(%rdi)
	        mov     %r15, 0x08(%rdi)
	        mov     %r14, 0x10(%rdi)
	        mov     %r13, 0x18(%rdi)
	        mov     %r12, 0x20(%rdi)
	        mov     %rbx, 0x28(%rdi)
	        mov     %rbp, 0x30(%rdi)
		
	        mov     0x00(%rsi), %rsp
	        mov     0x08(%rsi), %r15
	        mov     0x10(%rsi), %r14
	        mov     0x18(%rsi), %r13
	        mov     0x20(%rsi), %r12
	        mov     0x28(%rsi), %rbx
	        mov     0x30(%rsi), %rbp
		
	        ret