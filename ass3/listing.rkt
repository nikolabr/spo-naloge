#lang racket

(require "opcode.rkt")
(require "parser.rkt")
(require "emitter.rkt")
(require "asm.rkt")

(provide (all-from-out "opcode.rkt"))
(provide (all-from-out "parser.rkt"))
(provide (all-from-out "emitter.rkt"))
(provide (all-from-out "asm.rkt"))

(module* main #f
  (let ([in-filename (vector-ref (current-command-line-arguments) 0)]
        [out-filename (vector-ref (current-command-line-arguments) 1)])
    (display "Output file: ")
    (display out-filename)
    (display "\n")
    
    (print-file-listing in-filename out-filename)))
