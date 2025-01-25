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

;; Word (as integer) to bytes
(define (word-to-bytes word)
  (subbytes (integer->integer-bytes word 4 #f #t) 1))

(define (is-f4? l)
  (and (not (empty? l)) (list? (second l)) (member 'long (second l))))

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

          [(equal? first-elem "BYTE") (let ([b (parse-array (cdr rem))])
                                        (if (bytes? b) (bytes-length b) 1))]

          [(equal? first-elem "WORD") (let ([b (parse-array (cdr rem))])
                                        (if (bytes? b) (bytes-length b) 3))]
          
          [(equal? first-elem "RESB") (and (number? (last rem)) (* (last rem) 1))]
          [(equal? first-elem "RESW") (and (number? (last rem)) (* (last rem) 3))]

          [(equal? first-elem "ORG") (second l)]
          
          [else 0]))
      ))

(define (starts-with-label l)
  (let ([first-el (car l)])
    (and (string? first-el) (not (member first-el sicxe/directive-names)))))

(define (remove-label l)
  (if (and (not (empty? l))
           (starts-with-label l)) (cdr l) l))

(define (line-instr-length l)
  (if (empty? l)
      0
      (instr-length (remove-label l))))

(define (first-pass/process-line l res)
  (if (empty? l)
      res
      (let* ([prev (last res)]
             [is-label (starts-with-label l)]
             [label (if is-label (first l) "")]
             [len (line-instr-length l)])
        (append (drop-right res 1) (list (cons label prev) (+ prev len))))))

;; First pass of assembler, returns symbols
(define (first-pass ast)
  (let ([lines (drop-right (foldl first-pass/process-line (list 0) ast) 1)]
        [fn (lambda (i) (non-empty-string? (car i)))])
    (filter fn lines)))

(define (replace-right-symbol labels line)
  (if (empty? line)
      line
      (let* ([label (last line)]
             [location (dict-ref labels label #f)])
        (if location
            (append (drop-right line 1) (list location))
            line))))

;; Second pass of assembler
(define (second-pass labels ast)
  (map (lambda (i) (replace-right-symbol labels i)) ast))

(define (replace-opcode l)
  (let ([opcode (second l)])
    (if (symbol? opcode)
        (append (list (first l) (eval opcode) (cddr l)))
        #f)))

(define (process-instr l res)
  (let* ([prev (last res)]
         [len (instr-length l)])
    (append (drop-right res 1) (list (cons prev l) (+ prev len)))))

(define (get-format instr)
  (let ([opcode (first instr)]
        [is-long (and (>= (length instr) 2) (is-f4? instr))])
    (cond
      [(member opcode f1-opcodes) 'f1]
      [(member opcode f2-opcodes) 'f2]
      [(member opcode sic-opcodes) (if is-long 'f4 'f3)]
      [else #f]
      )))

(define (get-bp-mode pc len base modifier operand)
  (let* ([pc-after (+ pc len)]
         [pc-distance (- operand pc-after)])
    (cond
      [(member 'long modifier) (list 'mode-direct operand)]
      [(member 'base modifier) (list 'mode-base (- operand base))]
      [(and (>= pc-distance -2048)
            (<= pc-distance 2047))
       (list 'mode-pc-relative pc-distance)]
      [else (list 'mode-direct operand)])))

(define (get-ni-mode modifier)
  (cond
    [(member 'literal modifier) 'literal-mode]
    [(member 'indirect modifier) 'indirect-mode]
    [else 'simple-mode]))

(define (extended-bits modifier)
  (if (member 'long modifier) #x001000 #x000000))

(define (index-bits modifier)
  (if (member 'index modifier) #x008000 #x000000))

(define mode-direct #x000000)
(define mode-pc-relative #x002000)
(define mode-base #x004000)

(define literal-mode #x010000)
(define indirect-mode #x020000)
(define simple-mode #x030000)

(define (calculate-nixbpe-bits pc len base modifier operand)
  (let ([bp-mode (get-bp-mode pc len base modifier operand)]
        [ni-mode (get-ni-mode modifier)]
        [e (extended-bits modifier)]
        [x (index-bits modifier)])
    (bitwise-ior
     #x000000
     (eval (first bp-mode))
     (eval ni-mode)
     e
     x)))

(define reg-a 0)
(define reg-x 1)
(define reg-l 2)
(define reg-b 3)
(define reg-s 4)
(define reg-t 5)
(define reg-f 6)

(define (generate-f2 opcode operands)
  (let* ([to-symbol (lambda (n) (eval (string->symbol
                                       (string-append "reg-" (string-downcase n)))))]
         [first-op (to-symbol (first operands))]
         [second-op (if (> (length operands) 1)
                        (to-symbol (second operands))
                        0)])
    (bitwise-ior
     (arithmetic-shift opcode 8)
     (arithmetic-shift first-op 4)
     second-op
     ))
  )

(define (generate-f3 pc base modifier opcode operand)
  (let ([nixbpe (calculate-nixbpe-bits 3 pc base modifier operand)]
        [operand (second (get-bp-mode 3 pc base modifier operand))])
    (bitwise-ior
     (arithmetic-shift opcode 16)
     nixbpe
     (bitwise-and operand #xFFF))))

(define (generate-f4 pc base modifier opcode operand)
  ;; (display "F4\n")
  (let ([nixbpe (calculate-nixbpe-bits 4 pc base modifier operand)]
        [operand (second (get-bp-mode 4 pc base modifier operand))])
    (bitwise-ior
     (arithmetic-shift opcode 24)
     (arithmetic-shift nixbpe 8)
     (bitwise-and operand #xFFFFF)))
  )

(define (generate-instr l)
  (let ([pc (car l)]
        [instr (cdr l)])
    (match (get-format instr)
      ['f2 (generate-f2 (first instr) (last instr))]
      ['f3 (generate-f3 pc #f (take (last instr) 1) (car instr) (last (last instr)))]
      ['f4 (generate-f4 pc #f (take (last instr) 1) (car instr) (last (last instr)))]
      [_ (error "Unknown or unsupported format!")])))

(define (generate-code ast)
  (let* ([not-empty (filter (lambda (i) (not (empty? i))) ast)]
         [removed-labels (map remove-label not-empty)]
         [instr-locs (drop-right (foldl process-instr (list 0) removed-labels) 1)]
         [ops-with-locs (filter-map replace-opcode instr-locs)]
         [generated (map generate-instr ops-with-locs)]
         )
    generated))

(define (assemble p)
  (let* ([lines (sicxe/parse p)]
         [labels (first-pass lines)]
         [resolved (second-pass labels lines)]
         [generated (generate-code resolved)])
    generated))

