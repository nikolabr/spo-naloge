prog	START	0

	LDA	#64
	JSUB	scrfill

	JSUB	scrclear

halt	J	halt

scrcols	WORD	80	
scrrows	WORD	25
scrlen	WORD	2000

scrclear
	LDA	#32

scrfill
	STX	saved_x
	CLEAR	X

fillloop
	+STCH	screen, X

	TIX	scrlen
	JLT	fillloop
	
	LDX	saved_x
	RSUB

saved_x	RESW	1

	ORG	0xB800
screen	RESW	0

	END	prog
