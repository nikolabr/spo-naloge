#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)

(provide sicxe/lexer)
(provide sicxe/parser)
(provide sicxe/parse)
(provide sicxe/get-tokens)
(provide sicxe/directive-names)

(define sicxe/directive-names (list "START" "END" "ORG" "EQU" "BYTE" "WORD" "RESB" "RESW"))

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
     [(LITERAL SYMBOL) (list 'literal $2)]
     [(AT SYMBOL) (list 'indirect $2)]
     [(SYMBOL) $1]]

    [modifier
     [(PLUS) 'long]
     ]

    [array-expr
     [(SYMBOL SINGLEQUOTE SYMBOL SINGLEQUOTE) (list $1 $3)]]
    
    [line
     [(SPACE START SPACE SYMBOL) (list "START" $4)]
     [(SPACE END SPACE SYMBOL) (list "END" $4)]
     [(SPACE ORG SPACE SYMBOL) (list "ORG" $4)]
     
     [(SPACE EQU SPACE SYMBOL) (list "EQU" $4)]
     [(SPACE EQU SPACE ASTERISK) (list "EQU" "*")]

     [(SPACE BYTE SPACE SYMBOL) (list "BYTE" $4)]
     [(SPACE BYTE SPACE array-expr) (list "BYTE" $4)]
     
     [(SPACE WORD SPACE SYMBOL) (list "WORD" $4)]
     [(SPACE WORD SPACE array-expr) (list "WORD" $4)]

     [(SPACE RESB SPACE SYMBOL) (list "RESB" $4)]
     [(SPACE RESW SPACE SYMBOL) (list "RESW" $4)]
     
     [(SPACE INSTR SPACE symbol COMMA SPACE symbol) (list $2 $4 $7)]
     [(SPACE INSTR SPACE symbol) (list $2 $4)]
     [(SPACE INSTR) (list $2)]

     [(SPACE modifier INSTR SPACE symbol COMMA SPACE symbol) (list $3 $2 $5 $8)]
     [(SPACE modifier INSTR SPACE symbol) (list $3 $2 $5)]

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
