PROG	START	0

	LDX	X
	LDS	Y

	RMO	X, A
	ADDR	S, A
	STA	SUM

	RMO	X, A
	SUBR	S, A
	STA	DIFF

	RMO	X, A
	MULR	S, A
	STA	PROD
	
	RMO	X, A
	DIVR	S, A
	STA	QUOT

	RMO	X, A
MODLP	SUBR	S, A
	COMPR	A, S
	JGT	MODLP
	JEQ	MODLP

	STA MOD

HALT	J	HALT	
	
X	WORD	8
Y	WORD	3

SUM	RESW	1
DIFF	RESW	1
PROD	RESW	1
QUOT	RESW	1
MOD	RESW	1

	END	PROG
