#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)

(provide sicxe/lexer)
(provide sicxe/parser)
(provide sicxe/parse)
(provide sicxe/get-tokens)

(define-tokens basic-tokens (SYMBOL INSTR))
(define-empty-tokens directives (START END ORG EQU BYTE WORD RESB RESW))
(define-empty-tokens punct-tokens (NLINE EOF DOT SPACE COMMA ASTERISK MINUS PLUS LITERAL AT SINGLEQUOTE))

(define sicxe/lexer
  (lexer-src-pos
   ;; Whitespace, comments
   [(eof) (token-EOF)]
   ["\n" (token-NLINE)]
   [(concatenation (:+ blank) "\n") (token-NLINE)]
   [(:+ blank) (token-SPACE)]
   ["@" (token-AT)]
   ["." (token-DOT)]
   ["," (token-COMMA)]
   ["*" (token-ASTERISK)]
   ["-" (token-MINUS)]
   ["+" (token-PLUS)]
   
   ;; Directives
   ["START" (token-START)]
   ["END" (token-END)]
   ["ORG" (token-ORG)]
   ["EQU" (token-EQU)]
   
   ["BYTE" (token-BYTE)]
   ["WORD" (token-WORD)]

   ["RESB" (token-RESB)]
   ["RESW" (token-RESW)]

   [(union
     "ADD" "ADDF" "ADDR" "AND" "CLEAR"
     "COMP" "COMPF" "COMPR" "DIV" "DIVF"
     "DIVR" "FIX" "FLOAT" "HIO" "J"
     "JEQ" "JGT" "JLT" "JSUB" "LDA"
     "LDB" "LDCH" "LDF" "LDL" "LDS"
     "LDT" "LDX" "LPS" "MUL" "MULF"
     "MULR" "NORM" "OR" "RD" "RMO"
     "RSUB" "SHIFTL" "SHIFTR" "SIO"
     "SSK" "STA" "STB" "STCH" "STF"
     "STI" "STL" "STS" "STSW" "STT"
     "STX" "SUB" "SUBF" "SUBR" "SVC"
     "TD" "TIO" "TIX" "TIXR" "WD")
    (token-INSTR (string->symbol (string-append "op-" (string-downcase lexeme))))]
   
   ["#" (token-LITERAL)]
   ["'" (token-SINGLEQUOTE)]
   [(:+ numeric) (token-SYMBOL (string->number lexeme))]
   [(:+ (:or alphabetic numeric "_")) (token-SYMBOL lexeme)]
   ))

(define (split-lines l)
  (let*-values
      ([(p) (lambda (i) (not (equal? (token-name i) 'NLINE)))]
       [(line rem) (splitf-at l p)])
    (if (or (empty? l) (equal? l (list 'EOF)))
        (list)
        (append (list line) (split-lines (cdr rem))))))

(define (filter-empty-lines l)
  (filter (lambda (i) (not (empty? i))) l))

(define (sicxe/get-tokens p)
  (let ([lx (sicxe/lexer p)])
    (if (equal? (token-name (position-token-token lx)) 'EOF)
        (list lx)
        (append (list lx) (sicxe/get-tokens p)))))

(define (sicxe/get-lines p)
  (filter-empty-lines (split-lines (sicxe/get-tokens p))))

(define sicxe/parser
  (parser
   [start lines]
   [end EOF]
   [src-pos]
   [error (lambda (a b c d e) (begin (printf "a = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n" a b c d e) (void)))]
   [tokens basic-tokens punct-tokens directives]
   [grammar
    [comment-body
     [(comment-body SPACE SYMBOL) '()]
     [(SPACE SYMBOL) '()]
     [(SYMBOL) '()]]
    
    [symbol
     [(LITERAL SYMBOL) (cons 'literal $2)]
     [(AT SYMBOL) (cons 'indirect $2)]
     [(SYMBOL) $1]]

    [modifier
     [(PLUS) 'long]
     ]

    [array-expr
     [(SYMBOL SINGLEQUOTE SYMBOL SINGLEQUOTE) (cons $1 $3)]]
    
    [line
     [(SPACE START SPACE SYMBOL) (cons "START" $4)]
     [(SPACE END SPACE SYMBOL) (cons "END" $4)]
     [(SPACE ORG SPACE SYMBOL) (cons "ORG" $4)]
     
     [(SPACE EQU SPACE SYMBOL) (cons "EQU" $4)]
     [(SPACE EQU SPACE ASTERISK) (cons "EQU" "*")]

     [(SPACE BYTE SPACE SYMBOL) (cons "BYTE" $4)]
     [(SPACE BYTE SPACE array-expr) (cons "BYTE" $4)]
     
     [(SPACE WORD SPACE SYMBOL) (cons "WORD" $4)]
     [(SPACE WORD SPACE array-expr) (cons "WORD" $4)]

     [(SPACE RESB SPACE SYMBOL) (cons "RESB" $4)]
     [(SPACE RESW SPACE SYMBOL) (cons "RESB" $4)]
     
     [(SPACE INSTR SPACE symbol COMMA SPACE symbol) (list $2 $4 $7)]
     [(SPACE INSTR SPACE symbol) (cons $2 $4)]
     [(SPACE INSTR) (list $2)]

     [(SPACE modifier INSTR SPACE symbol COMMA SPACE symbol) (list $2 $3 $5 $8)]
     [(SPACE modifier INSTR SPACE symbol) (list $2 $3 $5)]

     [(SYMBOL line) (cons $1 $2)]

     [(SPACE) '()]
     [() '()]]
    [lines
     [(line NLINE lines) (append (list $1) $3)]
     [(line DOT comment-body NLINE lines) (append (list $1) $5)]
     [(line) (list $1)]
     ]
    ]
   ))

(define (sicxe/parse p)
  (sicxe/parser (lambda () (sicxe/lexer p))))
