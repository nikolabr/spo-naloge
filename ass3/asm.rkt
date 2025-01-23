#lang racket

(require "parser.rkt")

(define f1-opcodes (list))

;; F2
(define op-addr #x90)
(define op-clear #xB4)
(define op-compr #xA0)
(define op-divr #x9C)
(define op-mulr #x98)
(define op-rmo #xAC)
(define op-shiftl #xA4)
(define op-shiftr #xA8)
(define op-subr #x94)
(define op-tixr #xB8)

(define f2-opcodes
  (list op-addr op-clear op-compr
        op-divr op-mulr op-rmo
        op-shiftl op-shiftr op-subr))

;; SIC
(define op-add
  #x18)
(define op-and
  #x40)
(define op-comp
  #x28)
(define op-div
  #x24)
(define op-j
  #x3C)
(define op-jeq
  #x30)
(define op-jgt
  #x34)
(define op-jlt
  #x38)
(define op-jsub
  #x48)
(define op-lda
  #x00)
(define op-ldb
  #x68)
(define op-ldch
  #x50)
(define op-ldl
  #x08)
(define op-lds
  #x6C)
(define op-ldt
  #x74)
(define op-ldx
  #x04)
(define op-mul
  #x20)
(define op-or
  #x44)
(define op-rd
  #xD8)
(define op-rsub
  #x4C)
(define op-sta
  #x0C)
(define op-stch
  #x54)
(define op-stl
  #x14)
(define op-sts
  #x7C)
(define op-stsw
  #xE8)
(define op-stt
  #x84)
(define op-stx
  #x10)
(define op-sub
  #x1C)
(define op-td
  #xE0)
(define op-tix
  #x2C)
(define op-wd
  #xDC)

(define sic-opcodes
  (list op-add 
        op-and 
        op-comp
        op-div 
        op-j   
        op-jeq 
        op-jgt 
        op-jlt 
        op-jsub
        op-lda
        op-ldb
        op-ldch
        op-ldl 
        op-lds 
        op-ldt 
        op-ldx 
        op-mul 
        op-or  
        op-rd  
        op-rsub
        op-sta 
        op-stch
        op-stl
        op-sts
        op-stsw
        op-stt
        op-stx 
        op-sub 
        op-td  
        op-tix 
        op-wd
        ))

(define (is-f4? l)
  (and (not (empty? l)) (equal? (second l) 'long)))

(define (hex-array->bytes s)
  (let ([l (bytes->list (string->bytes/utf-8 s))]
        [fn (lambda (t)
              (cond
                [(and (>= t 48) (<= t 58)) (- t 48)]
                [(and (>= t 65) (<= t 70)) (+ (- t 65) 10)]
                [else (error "Unknown array")]
                ))])
    (list->bytes (map fn l))))

(define (parse-array s)
  (if (or (not (list? s)) (< (length s) 2))
      s
      (let ([type (first s)]
            [content (second s)])
        (match type
          ["C" (string->bytes/utf-8 content)]
          ["X" (hex-array->bytes content)]
          [_ (error "Unknown array type")]))))

(define (instr-length l)
  (if (empty? l)
      0
      (let* ([first-elem (if (or (list? l) (cons? l)) (car l) #f)]
         [opcode (if (symbol? first-elem)
                     (eval first-elem)
                     first-elem)]
         [is-long (and (>= (length l) 2) (is-f4? l))]
         [rem (remove 'long l)])
        (cond
          [(member opcode f1-opcodes) 1]
          [(member opcode f2-opcodes) 2]
          [(member opcode sic-opcodes) (if is-long 4 3)]
      
          [(member first-elem '("BYTE" "RESB"))
           (let ([b (parse-array (cdr rem))])
             (if (bytes? b) (bytes-length b) 1))]
      
          [(member first-elem '("WORD" "RESW"))
           (let ([b (parse-array (cdr rem))])
             (if (bytes? b) (bytes-length b) 3))]

          [(equal? first-elem "ORG") (second l)]
          
          [else 0]))
      ))

(define (process-line l res)
  (if (empty? l)
      res
      (let* ([prev (last res)]
             [first-elem (car l)]
             [is-first-string (string? first-elem)]
             [is-label (and is-first-string (not (member first-elem sicxe/directive-names)))]
             [label (if is-label first-elem "")]
             [len (instr-length (if is-label (cdr l) l))])
        (append (drop-right res 1) (list (cons label prev) (+ prev len))))))

;; First pass of assembler, returns symbols
(define (first-pass ast)
  (let ([lines (drop-right (foldl process-line (list 0) ast) 1)]
        [fn (lambda (i) (non-empty-string? (car i)))])
    (filter fn lines)))
