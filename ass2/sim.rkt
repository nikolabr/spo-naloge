#lang racket

(require "loader.rkt")

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

(define reg-mask #xFFFFFF)

(define (unsigned->signed len val)
  (let* ([mask (arithmetic-shift 1 (- len 1))]
         [msb (bitwise-and val mask)])
    (if (= msb 0)
        val
        (- (+ (bitwise-xor (bitwise-and val (- mask 1)) (- mask 1)) 1))
        )))

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
      '((a . 0)
        (x . 0)
        (l . 0)
        (b . 0)
        (s . 0)
        (t . 0)
        (f . 0)
        (pc . 0)
        (sw . 0)))

    (define mem-size (arithmetic-shift 1 20))
    (define mem (make-vector mem-size))

    (define/public (get-mem-size) mem-size)
    (define/public (get-mem) mem)

    (define/public (get-reg reg)
      (dict-ref regs reg))

    (define/public (set-reg reg word)
      (let ([masked-word (bitwise-and word reg-mask)])
        (set! regs (dict-set regs reg masked-word))))

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
      (let*
          ([b (word-to-bytes val)]
           [l (build-list 3 (lambda (i) (write-byte-at
                                         (+ addr i)
                                         (list-ref b i))))])
        l))
    
    (define/public (fetch)
      (let* ([old-pc (get-reg 'pc)]
             [val (read-byte-at old-pc)])
        (set-reg 'pc (+ old-pc 1))
        val))

    ;; Return immediate value or call f with the effective address if not immediate
    (define/public (call-effective addr f nixbpe-bits)
      (let* (
             ;; Fix address
             [fmt
              (match nixbpe-bits
                [(nixbpe _ _ _ _ _ 1) 'f4]
                [(nixbpe 0 0 _ _ _ _) 'sic]
                [_ 'f3])]
             [addr-len
              (match fmt
                ['f3 12]
                ['f4 20]
                ['sic 15]
                [_ (error "abc")])]
             [fixed-addr
              (match fmt
                ['f3 (unsigned->signed addr-len (bitwise-and addr #xFFF))]
                ['f4 (unsigned->signed addr-len (bitwise-ior (arithmetic-shift addr 8) (fetch)))]
                ['sic (unsigned->signed addr-len addr)]
                [_ (error "Unknown instruction format!")])
              ]
             [pc-val (get-reg 'pc)]
             [mask (- (arithmetic-shift 1 addr-len) 1)]
             [res (match nixbpe-bits
                    ;; Simple
                    [(nixbpe 1 1 0 0 0 _) (f fixed-addr)]
                    
                    [(nixbpe 1 1 0 0 1 0) (f (+ fixed-addr pc-val))]
                    [(nixbpe 1 1 0 1 0 0) (f (+ fixed-addr (get-reg 'b)))]
                    [(nixbpe 1 1 1 0 0 _) (f (+ fixed-addr (get-reg 'x)))]
                    
                    [(nixbpe 1 1 1 0 1 0) (f (apply + fixed-addr (get-reg 'x) pc-val))]
                    [(nixbpe 1 1 1 1 0 0) (f (apply + fixed-addr (get-reg 'x) (get-reg 'b)))]
                    
                    [(nixbpe 0 0 0 _ _ _) (f fixed-addr)]
                    [(nixbpe 0 0 1 _ _ _) (f (+ fixed-addr (get-reg 'x)))]
                    
                    ;; Indirect
                    [(nixbpe 1 0 0 0 0 _) (f (read-word-at fixed-addr))]
                    [(nixbpe 1 0 0 0 1 0) (f (+ (read-word-at fixed-addr) pc-val))]
                    [(nixbpe 1 0 0 1 0 0) (f (+ (read-word-at fixed-addr) (get-reg 'b)))]

                    ;; Immediate
                    [(nixbpe 0 1 0 0 0 _) fixed-addr]
                    [(nixbpe 0 1 0 0 1 _) (+ fixed-addr pc-val)]
                    [(nixbpe 0 1 0 0 0 _) (+ fixed-addr (get-reg 'b))]
                    
                    [_ (error "Invalid addressing mode")])])
        res
        ))

    (define (execute-f1 opcode) (error "F1 opcodes not implemented!"))
    
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
          [(== op-compr) (error "COMPR not implemented!")]
          [(== op-divr) (set-reg-index r1 (quotient r1-val r2-val))]
          [(== op-mulr) (set-reg-index r1 (* r1-val r2-val))]
          [(== op-rmo) (set-reg-index r1 r2-val)]
          [(== op-shiftl) (set-reg-index r1 (arithmetic-shift r1-val r2))]
          [(== op-shiftl) (set-reg-index r1 (arithmetic-shift r1-val (- r2)))]
          [_ (error "Unknown F2 opcode")])))
    
    (define (execute-comp reg offset nixbpe-bits)
      (let* ([read-f (lambda (addr) (read-word-at addr))]
             [r (get-reg reg)]
             [val (call-effective offset read-f nixbpe-bits)])
        (cond
          [(< r val) (set-reg 'sw #x00)]
          [(eq? r val) (set-reg 'sw #x40)]
          [(> r val) (set-reg 'sw #x80)])))

    ;; Arithmetic functions
    (define (execute-arith offset nixbpe-bits f)
      (let* ([a (get-reg 'a)]
             [read-f (lambda (addr) (read-word-at addr))]
             [res (f a (call-effective offset read-f nixbpe-bits))])
        (set-reg 'a res)))

    (define (execute-ldch offset nixbpe-bits)
      (let ([read-f (lambda (addr) (read-byte-at addr))])
        (set-reg 'a (call-effective offset read-f nixbpe-bits))))

    (define (execute-load reg offset nixbpe-bits)
      (let ([read-f (lambda (addr) (read-word-at addr))])
        (set-reg reg (call-effective offset read-f nixbpe-bits))))

    (define (execute-store reg offset nixbpe-bits)
      (let* ([val (get-reg reg)]
             [write-f (lambda (addr) (write-word-at addr val))]
             )
        (call-effective offset write-f nixbpe-bits)))

    (define (execute-stch offset nixbpe-bits)
      (let* ([a (get-reg 'a)]
             [write-f (lambda (addr) (write-byte-at addr))])
        (call-effective offset write-f nixbpe-bits)))

    (define (execute-rd offset nixbpe-bits)
      (let* ([read-f (lambda (addr) (read-byte-at addr))]
             [device-id (call-effective offset read-f nixbpe-bits)]
             [res (read-machine-device device-id)])
        (set-reg 'a res)))

    (define (execute-wd offset nixbpe-bits)
      (let* ([read-f (lambda (addr) (read-byte-at addr))]
             [device-id (call-effective offset read-f nixbpe-bits)]
             [b (bitwise-and (get-reg 'a) #xFF)])
        (write-machine-device device-id b)))
    
    (define (execute-tix offset nixbpe-bits)
      (set-reg 'x (+ (get-reg 'x) 1))
      (execute-comp 'x offset nixbpe-bits))

    (define (execute-jump offset nixbpe-bits)
      (set-reg 'pc (call-effective offset
                                   (lambda (addr) addr)
                                   nixbpe-bits)))
    
    (define (execute-conditional-jump expected-val offset nixbpe-bits)
      (when (= expected-val (get-reg 'sw))
        (execute-jump offset nixbpe-bits)))

    (define (execute-sic-f3-f4 b)
      (let* ([instr-word (bytes-to-word (list b (fetch) (fetch)))]
             [nixbpe-bits (decode-nixbpe-bits instr-word)]
             [offset (bitwise-and instr-word #x7FFF)]
             [opcode (bitwise-and b #xFC)]
             )
        (match opcode
          [(== op-add) (execute-arith offset nixbpe-bits +)]
          [(== op-and) (execute-arith offset nixbpe-bits bitwise-and)]
          [(== op-comp) (execute-comp 'a offset nixbpe-bits)]
          [(== op-div) (execute-arith offset nixbpe-bits quotient)]

          [(== op-j) (execute-jump offset nixbpe-bits)]
          
          [(== op-jeq) (execute-conditional-jump #x40 offset nixbpe-bits)]
          [(== op-jgt) (execute-conditional-jump #x34 offset nixbpe-bits)]
          [(== op-jlt) (execute-conditional-jump #x38 offset nixbpe-bits)]

          [(== op-jsub)
           (begin
             (set-reg 'l (get-reg 'pc))
             (execute-jump offset nixbpe-bits)
             )]

          [(== op-ldch) (execute-ldch)]
          [(== op-lda) (execute-load 'a offset nixbpe-bits)]
          [(== op-ldb) (execute-load 'b offset nixbpe-bits)]
          [(== op-ldl) (execute-load 'l offset nixbpe-bits)]
          [(== op-lds) (execute-load 's offset nixbpe-bits)]
          [(== op-ldt) (execute-load 't offset nixbpe-bits)]
          [(== op-ldx) (execute-load 'x offset nixbpe-bits)]

          [(== op-rd) (execute-rd offset nixbpe-bits)]
          [(== op-rsub) (set-reg 'pc (get-reg 'l))]
          
          [(== op-mul) (execute-arith offset nixbpe-bits *)]
          [(== op-or) (execute-arith offset nixbpe-bits bitwise-ior)]

          [(== op-sta) (execute-store 'a offset nixbpe-bits)]
          [(== op-stch) (execute-stch offset nixbpe-bits)]
          [(== op-stl) (execute-store 'l offset nixbpe-bits)]
          [(== op-sts) (execute-store 's offset nixbpe-bits)]
          [(== op-stsw) (execute-store 'sw offset nixbpe-bits)]
          [(== op-stt) (execute-store 't offset nixbpe-bits)]
          [(== op-stx) (execute-store 'x offset nixbpe-bits)]

          [(== op-sub) (execute-arith offset nixbpe-bits -)]

          [(== op-td) (error "TD not implemented")]
          [(== op-tix) (execute-tix offset nixbpe-bits)]
          [(== op-wd) (execute-wd offset nixbpe-bits)]
          
          [_ (error (format "SIC opcode ~a not implemented!" opcode))])))
    
    (define/public (execute)
      (let* ([b (fetch)]
             [opcode (bitwise-and b #xFC)]
             [pc-val (+ (get-reg 'pc) -1)]
             [f (cond
                  [(member opcode f2-opcodes) execute-f2]
                  [(member opcode sic-opcodes) execute-sic-f3-f4]
                  [else (error (format "Invalid opcode: ~a at ~a" opcode pc-val))]
                  )])
        ;; (print (format "PC: ~a, opcode: ~a" pc-val opcode))
        (f b)))

    (define/public (load-obj filename)
      (set-reg 'pc (load-section filename mem)))
    ))

(define (create-default-machine) (new machine%))

(provide machine%)

(module+ test
  (require rackunit)
  (check-equal? (send* (new machine%)
                  (write-word-at 0 #x00DEAD)
                  (read-word-at 0))
                #x00DEAD)
  
  ;; Test basic LDA
  (check-equal? (send* (new machine%)
                  ;; 	LDA   	V
                  ;; V	WORD	123
                  (write-word-at 0 #x032000)
                  (write-word-at 3 123)

                  (execute)
                  (get-reg 'a))
               123)

  ;; Test immediate
  (check-equal? (send* (new machine%)
                  (write-word-at 0 #x01007B)

                  (execute)

                  (get-reg 'a))
                123)
  
  ;; Test store then load
  (check-equal? (send* (new machine%)
                  (write-word-at 0 #x01007B)
                  (write-word-at 3 #x0F2006)
                  (write-word-at 6 #x010000)
                  
                  (execute)
                  (execute)
                  (execute)
                  
                  (get-reg 'a))
                0)  

  (check-equal? (send* (new machine%)
                  (write-word-at 0 #x010FFF)
                  
                  (execute)
                  
                  (get-reg 'a))
                4095)

  (check-equal? (send* (new machine%)
                  (write-word-at 0 #x3F2FFD)
                  (execute)
                  (get-reg 'pc)
                  )
                0)
  )
