#lang racket

(define (word-to-bytes word)
  (let ([offsets (list 16 8 0)]
        [f (lambda (arg) (bitwise-bit-field word arg (+ arg 8)))])
    (map f offsets)))

(define (bytes-to-word bytes)
  (match bytes
    [(list b2 b1 b0) (apply + (list
                               b0
                               (arithmetic-shift b1 8)
                               (arithmetic-shift b2 16)))]))

(define (get-input-port device-id)
  (match device-id
    [0 (current-input-port)]))

(define (get-output-port device-id)
  (match device-id
    [1 (current-output-port)]
    [2 (current-error-port)]))

;; Write byte to port
(define (write-machine-device device-id byte)
  (let ([device-port (get-output-port device-id)])
    (display byte device-port)
    (and (file-stream-port? device-port) (close-output-port device-port))))

;; Read one byte from port
(define (read-machine-device device-id)
  (let* ([device-port (get-input-port device-id)]
         [res (read-byte device-port)])
    (and (file-stream-port? device-port) (close-output-port device-port))
    res))

(define (error-not-implemented)
  (error "Not implemented"))

(define (error-invalid-opcode opcode)
  (error "Invalid opcode"))

(define (error-invalid-addressing) (error "Invalid addressing"))

(define reg-mask #xFFFFFF)

;; F2
(define op-addr #x58)
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
(define op-stsw
  #xE8)
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
        op-stsw
        op-stx 
        op-sub 
        op-td  
        op-tix 
        op-wd
        ))

(struct nixbpe (n i x b p e) #:transparent)

;; Takes 24-bit instruction word
(define (decode-nixbpe-bits instr)
  (let ([n (bitwise-bit-field instr 17 18)]
        [i (bitwise-bit-field instr 16 17)]
        [x (bitwise-bit-field instr 15 16)]
        [b (bitwise-bit-field instr 14 15)]
        [p (bitwise-bit-field instr 13 14)]
        [e (bitwise-bit-field instr 12 13)])
    (nixbpe n i x b p e)))

(define machine%
  (class object%
    (super-new)

    (define regs
      (list
       (list 'a 0)
       (list 'x 0)
       (list 'l 0)
       (list 'b 0)
       (list 's 0)
       (list 't 0)
       (list 'f 0)
       (list 'pc 0)
       (list 'sw 0)))

    (define mem-size 128)
    (define mem (make-vector mem-size))

    (define/public (get-reg reg)
      (assoc reg regs))

    (define/public (set-reg reg word)
      (let ([masked-word (bitwise-and word reg-mask)])
        (set! regs (dict-set regs reg word))))

    (define/public (get-reg-index index)
      (cdr (list-ref regs index)))

    (define/public (set-reg-index index word)
      (let* ([masked-word (bitwise-and word reg-mask)]
             [reg-name (car (list-ref regs index))])
        (set! regs (dict-set regs reg-name masked-word))))

    (define/public (read-byte-at addr) (vector-ref mem addr))
    (define/public (read-word-at addr) (bytes-to-word (build-list 3 (lambda (i) (read-byte-at (+ addr i))))))

    (define/public (write-byte-at addr val)
      (vector-set! mem addr val))
    
    (define/public (write-word-at addr val) 
      (let ([b (word-to-bytes val)]
            [l (build-list 3)])
        (map
         (lambda (i) (write-byte-at
                      (+ addr i)
                      (list-ref b i))) l)))
    
    (define/public (fetch)
      (let* ([old-pc (get-reg 'pc)]
             [val (read-byte-at old-pc)])
        (set-reg 'pc (+ old-pc 1))
        val))

    ;; Return immediate value or call f with the effective address if not immediate
    (define/public (call-with-effective-addr addr f nixbpe-bits)
      (let (
            ;; Fix address
            [fixed-addr
             (match nixbpe-bits
               [(nixbpe 0 0 _ _ _ _) addr]
               [(nixbpe _ _ _ _ _ 1) (bitwise-ior (arithmetic-shift addr 8) (fetch))]
               [_ (bitwise-and addr #x3FF)])
             ])
        (match nixbpe-bits
          ;; Simple
          [(nixbpe 1 1 0 0 0 _) (f fixed-addr)]
          
          [(nixbpe 1 1 0 0 1 0) (f (+ fixed-addr (get-reg 'pc)))]
          [(nixbpe 1 1 0 1 0 0) (f (+ fixed-addr (get-reg 'b)))]
          [(nixbpe 1 1 1 0 0 _) (f (+ fixed-addr (get-reg 'x)))]

          [(nixbpe 1 1 1 0 1 0) (f (apply + fixed-addr (get-reg 'x) (get-reg 'pc)))]
          [(nixbpe 1 1 1 1 0 0) (f (apply + fixed-addr (get-reg 'x) (get-reg 'b)))]

          [(nixbpe 0 0 0 _ _ _) (f fixed-addr)]
          [(nixbpe 0 0 1 _ _ _) (f (+ fixed-addr (get-reg 'x)))]
          
          ;; Indirect
          [(nixbpe 1 0 0 0 0 _) (f (read-word-at fixed-addr))]
          [(nixbpe 1 0 0 0 1 0) (f (+ (read-word-at fixed-addr) (get-reg 'pc)))]
          [(nixbpe 1 0 0 1 0 0) (f (+ (read-word-at fixed-addr) (get-reg 'b)))]

          ;; Immediate
          [(nixbpe 0 1 0 0 0 _) fixed-addr]
          [(nixbpe 0 1 0 0 1 _) (+ fixed-addr (get-reg 'pc))]
          [(nixbpe 0 1 0 0 0 _) (+ fixed-addr (get-reg 'b))]
          
          [_ (error "Invalid addressing mode")])))

    (define (execute-f1 opcode) (error-not-implemented))

    (define (execute-f2 opcode)
      (let* ([operand (fetch)]
             [r1 (bitwise-bit-field operand 4 8)]
             [r2 (bitwise-bit-field operand 0 4)]
             [r1-val (get-reg-index r2)]
             [r2-val (get-reg-index r2)]
             )
        (match opcode
          [(== op-addr) (set-reg-index r1 (+ r1-val r2-val))]
          [(== op-clear) (set-reg-index r1 0)]
          [(== op-compr) (error-not-implemented)]
          [(== op-divr) (set-reg-index r1 (quotient r1-val r2-val))]
          [(== op-mulr) (set-reg-index r1 (* r1-val r2-val))]
          [(== op-rmo) (set-reg-index r1 r2-val)]
          [(== op-shiftl) (set-reg-index r1 (arithmetic-shift r1-val r2))]
          [(== op-shiftl) (set-reg-index r1 (arithmetic-shift r1-val (- r2)))]
          [_ (error-not-implemented)])))

    (define (execute-sic-f3-f4 opcode)
      (let* ([instr-word (bytes-to-word (list opcode (fetch) (fetch)))]
             [nixbpe-bits (decode-nixbpe-bits instr-word)]
             [offset (bitwise-and instr-word #x7FFF)]
             )
        (match opcode
          [_ (error-not-implemented)])))
    
    (define/public (execute)
      (let* ([opcode (fetch)]
             [f (cond
                  [(member opcode f2-opcodes) execute-f2]
                  [(member opcode sic-opcodes) execute-sic-f3-f4]
                  [#t (error-not-implemented)])])
        (f opcode)))
    ))

(define (create-default-machine) (new machine%))
