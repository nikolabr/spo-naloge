#lang racket

(define (parse-head-record line)
  (let* ([ls (string-split line)])
    (match ls
      [(list "H" program-name code-addr code-len)
       (string->number (string-append "#x" code-addr))]
      [_ (error "Head record is invalid!")])))

(define (read-head-record ip)
  (parse-head-record (read-line ip)))

(define (hex-string->num s) (string->number (string-append "#x" s)))

(define (parse-end-record line)
  (let* ([ls (string-split line)])
    (match ls
      [(list "E" start-addr)
       (string->number (string-append "#x" start-addr))]
      [_ (error "End record is invalid!")])))

(define (read-end-record ip)
  (parse-end-record (read-line ip)))

(define (parse-text-start s)
  (let ([ls (string-split s)])
    (match ls
      [(list "T" code-addr code-len) (hex-string->num code-addr)]
      [_ #f])))

(define (parse-element code-addr vec elem)
  (let* ([l (string-length elem)]
         [nb (quotient l 2)])
    (for ([i nb])
      (let* ([ss (substring elem (* i 2) (+ (* i 2) 2))]
             [x (hex-string->num ss)])
        (vector-set! vec (+ code-addr i) x)))
    nb))

(define (parse-text-record line vec)
  (let ([start-addr (parse-text-start (substring line 0 11))])
    (if (false? start-addr)
        #f
        (let ([str-elems (string-split (substring line 11))]
              [f (lambda (el addr)
                   (+ addr (parse-element addr vec el)))])
          (foldl f start-addr str-elems)))))

(define (read-text-record ip vec)
  (let* ([v (parse-text-record (read-line ip) vec)])
    (if (false? v)
        v
        (read-text-record ip v))))

;; Takes memory vec as argument, returns new PC
(define (load-section filename vec)
  (let* ([ip (open-input-file filename)])
    (read-head-record ip)
    (read-text-record ip vec)
    (read-end-record ip))
  )
