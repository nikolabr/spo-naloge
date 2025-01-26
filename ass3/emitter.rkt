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
    (string-upcase (string-append "H" name-str addr-str len-str))))

(define (create-text-record code-addr b)
  (let ([addr-str (~r code-addr
                      #:base 16
                      #:min-width 6
                      #:pad-string "0")]
        [len-str (~r (bytes-length b)
                     #:base 16
                     #:min-width 2
                     #:pad-string "0")]
        [bytes-str (apply string-append
                          (map (lambda (i) (~r i #:base 16 #:min-width 2 #:pad-string "0"))
                               (bytes->list b)))])
    (string-upcase (string-append "T" addr-str len-str bytes-str))))

(define (create-mod-record addr len)
  (let ([addr-str (~r addr
                      #:base 16
                      #:min-width 6
                      #:pad-string "0")]
        [len-str (~r len
                     #:base 16
                     #:min-width 2
                     #:pad-string "0")])
    (string-upcase (string-append "M" addr-str len-str))))

(define (create-end-record start-addr)
  (let ([addr-str (~r start-addr
                      #:base 16
                      #:min-width 6
                      #:pad-string "0")])
    (string-upcase (string-append "E" addr-str))))

(define (print-instruction instr p)
  (display (create-text-record (car instr) (cdr instr)) p)
  (display "\n" p))

(define (print-mod-record instr p)
  (display (create-mod-record (first instr) (second instr)) p)
  (display "\n" p))

(define (create-object-file p name code-addr code-length start-addr instructions mod-records)
  (display (create-head-record name code-addr code-length) p)
  (display "\n" p)
  (map (lambda (i) (print-instruction i p)) instructions)
  (map (lambda (i) (print-mod-record i p)) mod-records)
  (display (create-end-record start-addr) p)
  )
