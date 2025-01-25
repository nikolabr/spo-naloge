#lang racket

(provide create-head-record)
(provide create-text-record)
(provide create-end-record)

(provide create-object-file)

(define (create-head-record name code-addr code-length)
  (let ([name-str (~a name
                      #:min-width 6
                      #:max-width 6
                      #:align 'right)]
        [addr-str (~r code-addr
                      #:base 16
                      #:min-width 6
                      #:pad-string "0")]
        [len-str (~r code-length
                     #:base 16
                     #:min-width 6
                     #:pad-string "0")])
    (string-append "H" name-str addr-str len-str)))

(define (create-text-record code-addr b)
  (let ([addr-str (~r code-addr
                      #:base 16
                      #:min-width 6
                      #:pad-string "0")]
        [len-str (~r (bytes-length b)
                     #:base 16
                     #:min-width 2
                     #:pad-string "0")]
        [bytes-str (string-upcase (apply string-append
                                         (map (lambda (i) (~r i #:base 16 #:min-width 2 #:pad-string "0"))
                                              (bytes->list b))))])
    (string-append "T" addr-str len-str bytes-str)))

(define (create-end-record start-addr)
  (let ([addr-str (~r start-addr
                      #:base 16
                      #:min-width 6
                      #:pad-string "0")])
    (string-append "E" addr-str)))

(define (print-instruction instr p)
  (display (create-text-record (car instr) (cdr instr)) p)
  (display "\n" p))

(define (create-object-file p name code-addr code-length start-addr instructions)
  (display (create-head-record name code-addr code-length) p)
  (display "\n" p)
  (map (lambda (i) (print-instruction i p)) instructions)
  (display (create-end-record start-addr) p)
  )
