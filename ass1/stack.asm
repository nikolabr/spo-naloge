prog	START	0

	LDA	#1
	LDB	#2

	JSUB	stackinit

	LDA	#5
	JSUB	fakulteta

	LDA	#6
	JSUB	fakulteta

halt	J	halt

fakulteta
	STL	@sp
	JSUB	stackpush

	COMP	#0
	JGT	fakulteta1

	LDA	#1
	J	fakulteta_exit

fakulteta1
	STA	@sp
	JSUB	stackpush

	SUB	#1

	JSUB	fakulteta

	JSUB	stackpop
	MUL	@sp
	
fakulteta_exit
	JSUB	stackpop
	LDL	@sp

	RSUB

stackinit
	STA	saved_a

	LDA	#stack
	ADD	#1000
	SUB	#3
	STA	sp

	LDA	saved_a
	RSUB	

stackpop
	STA	saved_a

	LDA	sp
	SUB	#3
	STA	sp

	LDA	saved_a
	RSUB

stackpush
	STA	saved_a

	LDA	sp
	ADD	#3
	STA	sp

	LDA	saved_a
	RSUB

saved_a	WORD	0


sp	WORD	0
stack	RESW	1000
	
	END prog
