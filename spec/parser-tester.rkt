#lang br
(require "parser.rkt" "tokenizer.rkt" brag/support)
;(parse-to-datum "(q0)-a->(q1)")

;(make-tokenizer "(q0)-a->(q1)")
;(apply-tokenizer-maker make-tokenizer "=>(q0)-eps->(q0)\n(q0)-a->(q1)")
(parse-to-datum (apply-tokenizer-maker make-tokenizer "=>(q0)-eps->(q0)\n(q0)-a->(q1)"))