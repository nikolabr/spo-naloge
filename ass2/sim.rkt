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
(define op-add #x18)
(define op-and #x40)
(define op-lda #x00)

(define sic-opcodes
  (list op-add op-and op-lda))

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

    (define (fetch-byte addr) (vector-ref mem addr))
    (define (fetch-word addr)
      (bytes-to-word (build-list 3 (lambda (i) (fetch-byte (+ addr i))))))

    (define/public (fetch)
      (let* ([old-pc (get-reg 'pc)]
             [val (fetch-byte old-pc)])
        (set-reg 'pc (+ old-pc 1))
        val))

    (define/public (get-effective-addr addr nixbpe-bits) (list))

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
             [addr-bits (decode-nixbpe-bits instr-word)])
        (match opcode
          [_ (error-not-implemented)])))
    
    (define/public (execute)
      (let* ([opcode (fetch)]
             [f (cond
                  [(member opcode f2-opcodes) execute-f2]
                  [#t (error-not-implemented)])])
        (f opcode)))
    ))

(define (create-default-machine) (new machine%))
