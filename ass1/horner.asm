primer	START	0

PROG	CLEAR	A
	CLEAR	X

	LDX	#N
	
LOOP	
	. Decrement X reg
	LDS	#3
	SUBR	S, X
	
	. Multiply previous value with x
	LDA	TOTAL
	
	MUL	x
	ADD	P, X

	STA	TOTAL
	
	RMO	X, A
	COMP	#0
	JGT	LOOP
	

HALT	J	HALT
	
x	WORD	2	

P	WORD	5
	WORD	4
	WORD	3
	WORD	2
	WORD	1

PN	EQU	*
N	EQU	PN - P

TOTAL	WORD	0
	
	END	PROG
