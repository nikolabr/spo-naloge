primer	START	0

PROG	CLEAR	X
	CLEAR	A

LOOP	LDCH	STR, X
	WD	#0xAA
		
	TIX	#LEN

	JLT	LOOP

HALT	J	HALT
	
STR	BYTE	C'SIC/XE'
	BYTE	10
LAST	EQU	*
LEN	EQU	LAST - STR

	END	PROG
