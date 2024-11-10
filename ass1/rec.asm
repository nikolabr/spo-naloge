prog	START	0

	LDA	#1
	LDB	#2

	JSUB	stackinit

loop
	JSUB	readnum

	COMP	#0
	JEQ	halt
	
	JSUB	fakulteta
	JSUB	num

	LDA	#10
	WD	#1
	
	J	loop

halt	J	halt

readnum
	CLEAR	A
	CLEAR	S

	RMO	A, S

readnum_loop
	RD	#0xFA

	COMP	#48
	JLT	readnum_exit
	
	SUB	#48
	ADDR	A, S

	J	readnum_loop

readnum_exit
	RMO	S, A

	RSUB

		. Write C string
string		STA	string_addr
		STA	string_start
string_loop	
		CLEAR	A
		LDCH	@string_addr
		COMP	#0
		JEQ	string_exit

		WD	#1

		LDA	string_addr
		ADD	#1
		STA	string_addr

		J	string_loop
	
string_exit	LDA	string_start
		RSUB

string_addr	RESW	1
string_start	RESW	1


num		STA	num_saved_a
		STA	num_saved_x
		
		STA	num_l1
		LDX	#21

		. Write digits
num_loop
		LDA	num_l1

		DIV	#10
		MUL	#10
		STA	num_l2
		
		LDA	num_l1
		SUB	num_l2
		STA	num_digits, X
		
		LDA	num_l1
		DIV	#10
		STA	num_l1

		RMO	X, A
		COMP	#0
		JEQ	num_convert

		SUB	#3
		RMO	A, X
		J	num_loop

num_convert
		. Convert to string
		LDX	#0
		
num_con_loop	
		STX	num_l1
		RMO	X, A
		MUL	#3
		RMO	A, X
		
		LDA	num_digits, X
		
		ADD	#48

		LDX	num_l1
		STCH	num_str, X
		
		TIX	#8
		JLT	num_con_loop

		J	num_exit
		
num_exit	LDX	num_saved_x

		STL	num_saved_x
		LDA	#num_str
		JSUB	string
		LDL	num_saved_x

		LDA	num_saved_a

		RSUB
			
num_saved_a	RESW	1
num_saved_x	RESW	1
		
		. Store the digits here
num_digits	RESW	8
		
		. Store the output string here
num_str		RESB	8
		BYTE	0

num_l1		RESW	1
num_l2		RESW	1

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
