#lang racket

(require "opcode.rkt")
(require "parser.rkt")
(require "emitter.rkt")

(provide (all-from-out "opcode.rkt"))
(provide (all-from-out "parser.rkt"))
(provide (all-from-out "emitter.rkt"))

(define mode-direct #x000000)
(define mode-pc-relative #x002000)
(define mode-base #x004000)

(define literal-mode #x010000)
(define indirect-mode #x020000)
(define simple-mode #x030000)

(provide mode-direct mode-pc-relative mode-base
         literal-mode indirect-mode simple-mode)

(define reg-a 0)
(define reg-x 1)
(define reg-l 2)
(define reg-b 3)
(define reg-s 4)
(define reg-t 5)
(define reg-f 6)

(provide reg-a reg-x reg-l reg-b reg-s reg-t reg-f)

(provide assemble-file)
(provide print-file-listing)

(define base-addr #f)

;; Word (as integer) to bytes
(define (word-to-bytes word)
  (subbytes (integer->integer-bytes word 4 #f #t) 1))

(define (is-f4? l)
  (and (not (empty? l)) (list? l) (member 'long l)))

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

(define (instr-length l prev)
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

          [(equal? first-elem "ORG") (- (second l) prev)]
          
          [else 0]))
      ))

(define (starts-with-label l)
  (let ([first-el (car l)])
    (and (string? first-el)
         (not (member first-el sicxe/directive-names)))))

(define (remove-label l)
  (if (and (not (empty? l))
           (starts-with-label l)) (cdr l) l))

(define (line-instr-length l prev)
  (if (empty? l)
      0
      (instr-length (remove-label l) prev)))

(define (first-pass/process-line l res)
  (if (or (empty? l)
          (member "EQU" l))
      res
      (let* ([prev (last res)]
             [is-label (starts-with-label l)]
             [label (if is-label (first l) "")]
             [len (line-instr-length l prev)])
        (append (drop-right res 1) (list (cons label prev) (+ prev len))))))

;; First pass of assembler, returns symbols
(define (first-pass ast)
  (let ([lines (drop-right (foldl first-pass/process-line (list 0) ast) 1)]
        [fn (lambda (i) (non-empty-string? (car i)))])
    (filter fn lines)))

(define (replace-right-symbol labels line)
  (if (empty? line)
      line
      (let* ([last-el (last line)]
             [label (if (list? (last line))
                        (last last-el)
                        last-el)]
             [label-modifier (if (list? (last line))
                                 (list (first last-el))
                                 (list))]
             [location (dict-ref labels label #f)]
             [opcode (first line)])
        (if (and location
                 (symbol? opcode)
                 (not (member (eval opcode) f2-opcodes))
                 (not (member "EQU" line)))
            (append (drop-right line 1)
                    (list (append label-modifier (list location))))
            line))))

(define (get-literals ast)
  (let ([fn (lambda (i)
              (match i
                [(list label "EQU" num) (cons label num)]
                [_ #f]))])
    (filter-map fn ast)))

;; Second pass of assembler
(define (second-pass labels ast)
  (map (lambda (i) (replace-right-symbol labels i)) ast))

(define (replace-opcode l)
  (let ([opcode (second l)])
    (cond
      [(symbol? opcode) (append (take l 1) (list (eval opcode)) (drop l 2))]
      [(equal? opcode "BASE") l]
      [(equal? opcode "NOBASE") l]
      [else #f])
    ))

(define (process-instr l res)
  (let* ([prev (last res)]
         [len (instr-length l prev)])
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

(define (get-bp-mode pc len base modifier operands)
  (let* ([pc-after (+ pc len)]
         [operand (cond
                    [(list? operands) (last operands)]
                    [else operands])])
    (cond
      [(and (list? operands)
            (member 'literal operands)) (list 'mode-direct operand)]
      [(member 'long modifier) (list 'mode-direct operand)]
      [(and base
            (>= (- operand base) 0)
            (< (- operand base) 4096))
       (list 'mode-base (- operand base))]
      [(and (>= (- operand pc-after) -2048)
            (<= (- operand pc-after) 2047))
       (list 'mode-pc-relative (- operand pc-after))]
      [(and (>= operand 0)
            (< operand 4096))
       (list 'mode-direct operand)]
      [else (error "Operand invalid")])))

(define (get-ni-mode modifier)
  (cond
    [(not (list? modifier)) 'simple-mode]
    [(member 'literal modifier) 'literal-mode]
    [(member 'indirect modifier) 'indirect-mode]
    [else 'simple-mode]))

(define (extended-bits modifier)
  (if (member 'long modifier) #x001000 #x000000))

(define (index-bits modifier)
  (if (and (list? modifier)
           (member 'index modifier)) #x008000 #x000000))



(define (calculate-nixbpe-bits pc len base modifier operand)
  (let ([bp-mode (get-bp-mode pc len base modifier operand)]
        [ni-mode (get-ni-mode operand)]
        [e (extended-bits modifier)]
        [x (index-bits operand)])
    (bitwise-ior
     #x000000
     (eval (first bp-mode))
     (eval ni-mode)
     e
     x)))



(define (generate-f2 opcode operands)
  (let* ([to-symbol (lambda (n) (eval (string->symbol
                                       (string-append "reg-" (string-downcase n)))))]
         [first-op (to-symbol (first operands))]
         [second-op (if (> (length operands) 1)
                        (to-symbol (second operands))
                        0)])
    (integer->integer-bytes
     (bitwise-ior
      (arithmetic-shift opcode 8)
      (arithmetic-shift first-op 4)
      second-op
      )
     2 #f #t))
  )

(define (generate-f3 pc base modifier opcode operand)
  (let ([nixbpe (calculate-nixbpe-bits 3 pc base modifier operand)]
        [operand (match opcode
                   [(== op-rsub) #x0]
                   [_ (second (get-bp-mode 3 pc base modifier operand))] )])
    (subbytes
     (integer->integer-bytes
      (bitwise-ior
       (arithmetic-shift opcode 16)
       nixbpe
       (bitwise-and operand #xFFF))
      4 #f #t) 1)))

(define (generate-f4 pc base modifier opcode operand)
  (let ([nixbpe (calculate-nixbpe-bits 4 pc base modifier operand)]
        [operand (second (get-bp-mode 4 pc base modifier operand))])
    (integer->integer-bytes
     (bitwise-ior
      (arithmetic-shift opcode 24)
      (arithmetic-shift nixbpe 8)
      (bitwise-and operand #xFFFFF))
     4 #f #t))
  )

(define (generate-instr l)
  (cond
    [(equal? (second l) "BASE") (set! base-addr (last l))
                                #f]
    [(equal? (second l) "NOBASE") (set! base-addr #f)
                                  #f]
    [else (let* ([pc (car l)]
           [instr (cdr l)]
           [operands (last instr)]
           [instr-modifiers (if (> (length instr) 1)
                                (list (second instr))
                                (list))])
            (cons pc
            (match (get-format instr)
              ['f2 (generate-f2 (first instr) (cdr instr))]
              ['f3 (generate-f3 pc base-addr instr-modifiers (car instr) operands)]
              ['f4 (generate-f4 pc base-addr instr-modifiers (car instr) operands)]
              [_ (error "Unknown or unsupported format!")])))]
    ))

(define (generate-mod-record l)
  (let* ([pc (car l)]
         [instr (cdr l)]
         [operands (last instr)]
         [instr-modifiers (if (> (length instr) 1)
                              (list (second instr))
                              (list))]
         [len (match (get-format instr)
                   ['f3 3]
                   ['f4 4]
                   [_ 0])]
         [mode (if (> len 0)
                   (get-bp-mode pc len #f instr-modifiers operands)
                   (list))]
         [record-len (match len
                       [3 3]
                       [4 5]
                       [_ 0])])
    (if (and (member 'mode-direct mode)
             (list? operands)
             (not (member 'literal operands)))
        (list (+ pc 1)
              record-len)
        #f)))

(define (generate-code ast)
  (let* ([removed-labels (map remove-label ast)]
         [instr-locs (drop-right (foldl process-instr (list 0) removed-labels) 1)]
         [ops-with-locs (filter-map replace-opcode instr-locs)]
         )
    (filter-map generate-instr ops-with-locs)))

(define (generate-mod-records ast)
  (let* ([removed-labels (map remove-label ast)]
         [instr-locs (drop-right (foldl process-instr (list 0) removed-labels) 1)]
         [ops-with-locs (filter-map replace-opcode instr-locs)])
    (filter-map generate-mod-record ops-with-locs)))

(define (parse-variable line)
  (let ([pc (first line)])
    (match (cdr line)
      [(list "WORD" arr)
       (let ([a (parse-array arr)])
         (cons pc
               (if (bytes? a)
                   a
                   (subbytes (integer->integer-bytes a 4 #f #t) 1))))]
      [(list "BYTE" arr)
       (let ([a (parse-array arr)])
         (cons pc
               (if (bytes? a)
                   a
                   (bytes a))))]

      [(list "RESB" n) (cons pc (make-bytes n))]
      [(list "RESW" n) (cons pc (make-bytes (* n 3)))]
      
      [_ #f])))

(define (generate-variables ast)
   (let* ([removed-labels (map remove-label ast)]
          [instr-locs (drop-right (foldl process-instr (list 0) removed-labels) 1)])
     (filter-map parse-variable
                 instr-locs)))

(define (assemble p o)
  (let* ([lines (filter (lambda (i) (not (empty? i))) (sicxe/parse p))]
         [labels (append (first-pass lines)
                         (get-literals lines))]
         [resolved (second-pass labels lines)]
         [code (generate-code resolved)]
         [variables (generate-variables lines)]
         [mod-records (generate-mod-records resolved)])
    (create-object-file o
                        ""
                        0
                        (max (car (last code))
                             (if (empty? variables)
                                 0
                                 (car (last variables))))
                        0
                        (append code
                                variables)
                        mod-records)))

(define (assemble-file in-filename out-filename)
  
  
  (with-output-to-file out-filename
    #:exists 'replace
    (lambda ()
      (with-input-from-file in-filename
        (lambda ()
          (assemble (current-input-port)
                    (current-output-port)))))))

(define (display-listing l o lines text)
  (display (~r (car l) #:base 16 #:min-width 6 #:pad-string "0") o)
  (display " " o)
  (display (apply string-append
                  (map (lambda (i)
                         (~r i #:base 16 #:min-width 2 #:pad-string "0"))
                       (bytes->list (cdr l)))) o)
  (display "\n" o)
  ;; (let ([m (assoc (car l) lines)])
  ;;   (if m
  ;;       (display (list-ref text (cdr m)) o)
  ;;       #f))
  (map (lambda (i)
         (display "\t")
         (display (list-ref text (cdr i)))
         (display "\n"))
       (filter
        (lambda (i)
          (= (car i) (car l)))
        lines))
  (display "\n" o)
  )

(define (print-listing p o text)
  (let* ([lines (filter (lambda (i) (not (empty? i))) (sicxe/parse p))]
         [labels (append (first-pass lines)
                         (get-literals lines))]
         [resolved (second-pass labels lines)]
         [line-location (drop-right (foldl (lambda (l res)
                                             (let* ([prev (last res)]
                                                    [len (line-instr-length l prev)])
                                               (append
                                                (drop-right res 1)
                                                (list (cons prev
                                                            (- (length res) 1)))
                                                (list (+ prev len))))
                                             ) (list 0)
                                           resolved) 1)]
         [code (generate-code resolved)]
         [variables (generate-variables lines)])
    (map (lambda (i) (display-listing i o line-location text))
         (append code variables)))
  #t)
        
(define (read-port-lines)
  (let ([l (read-line)])
    (if (equal? l eof)
        (list)
        (append (list l)
                (read-port-lines)))))

(define (get-file-lines in-file)
  (filter
   (lambda (i) (and
                (not (empty? i))
                (not (regexp-match-exact? #rx"[ \t\n]*" i))))
   (with-input-from-file in-file
     (lambda ()
       (let ([text (read-port-lines)])
         text)))))

(define (print-file-listing in-filename out-filename)
  (let ([in-text (get-file-lines in-filename)])
    (with-output-to-file out-filename
      #:exists 'replace
      (lambda ()
        (with-input-from-file in-filename
          (lambda ()
            (print-listing (current-input-port)
                           (current-output-port)
                           in-text)))))))

(module* main #f
  (let ([in-filename (vector-ref (current-command-line-arguments) 0)]
        [out-filename (vector-ref (current-command-line-arguments) 1)])
    (display "Output file: ")
    (display out-filename)
    (display "\n")
    
    (assemble-file in-filename out-filename))
  )
