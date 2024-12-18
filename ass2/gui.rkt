#lang racket

(require "sim.rkt")
(require racket/gui)

(define (hex-string->num s) (string->number (string-append "#x" s)))

(define sim (new machine%))

(define frame (new frame%
                   [label "Racket SIC-Xe sim"]
                   [width 1024]
                   [height 1024]))

(define reg-names (list "A" "X" "L" "B" "S" "T" "F" "PC" "SW"))

(define reg-fields (map (lambda (name)
                          (new text-field%
                               [label name]
                               [parent frame]
                               [init-value "0"]
                               ))
                        reg-names))

(define mem-view-addr 0)

(define (read-mem start-addr)
  (for/list ([i (in-range start-addr (+ start-addr 144))])
    (let* ([fs (cond
                 [(= (modulo (+ (- i start-addr) 1) 12) 0) "~x~x\n"]
                 [(= (modulo (+ (- i start-addr) 1) 3) 0) "~x~x "]
                 [else "~x~x"]
                     )]
           [x (send sim read-byte-at i)]
           [x1 (bitwise-bit-field x 4 8)]
           [x2 (bitwise-bit-field x 0 4)])
      (format fs x1 x2))
    ))

(define (update-registers)
  (for ([i (in-range (length reg-names))])
    (let ([reg-field (list-ref reg-fields i)])
      (send reg-field set-value
            (format "~a" (send sim get-reg-index i))))))

(define (update-mem-view)
  (send text erase)
  (send text insert (apply string-append (read-mem mem-view-addr)) 0))

(define (update-machine)
  (update-registers)
  (update-mem-view)
  )

(define panel (new horizontal-panel%
                       [parent frame]
                       [alignment '(center center)]))

(define reset-button (new button%
                          [parent panel]
                          [label "Reset"]
                          [callback (lambda (button event)
                                      (set! sim (new machine%))
                                      (update-machine))]))

(define (load-object-file)
  (get-file))

(define load-button (new button%
                          [parent panel]
                          [label "Load"]
                          [callback (lambda (button event)
                                      (let ([filename (load-object-file)])
                                        (send sim load-obj filename)
                                        (update-machine)
                                        ))]))

(define step-button (new button%
                         [parent panel]
                         [label "Step"]
                         [callback (lambda (button event)
                                     (send sim execute)
                                     (update-machine))]))

(define mem-addr-text-field (new text-field%
                                 [parent frame]
                                 [label "Address"]
                                 [init-value (format "~x" mem-view-addr)]
                                 [callback (lambda (tfield event)
                                             (let ([val (hex-string->num (send tfield get-value))])
                                               (set! mem-view-addr val)
                                               (update-mem-view)))
                                           ]))

(define editor-canvas (new editor-canvas%
                           (parent frame)
                           (label "Editor Canvas")))

(define text (new text%))
(send editor-canvas set-editor text)

(define step-timer (new timer%
                        [interval #f]
                        [notify-callback (lambda ()
                                    (send sim execute)
                                    (update-machine))]))


(define start-button (new button%
                         [parent panel]
                         [label "Start"]
                         [callback (lambda (button event)
                                     (send step-timer start 50))]))

(define stop-button (new button%
                         [parent panel]
                         [label "Stop"]
                         [callback (lambda (button event)
                                     (send step-timer stop))]))


(send frame show #t)

