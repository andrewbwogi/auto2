#lang br
(require "parser.rkt" "tokenizer.rkt" brag/support)
;(parse-to-datum "node v0 meth(main) entry")

;(make-tokenizer "node v0 meth(main) entry")
;(apply-tokenizer-maker make-tokenizer "node v0 meth(main) entry\nnode v3 meth(a) entry\nedge v0 v1 eps")
;(parse-to-datum (apply-tokenizer-maker make-tokenizer "node v0 meth(main) entry"))
;(apply-tokenizer-maker make-tokenizer "node v0 meth(main) entry")
;(parse-to-datum (apply-tokenizer-maker make-tokenizer "node v0 meth(main) entry"))
(parse-to-datum (apply-tokenizer-maker make-tokenizer "node v0 meth(main) entry\nnode v3 meth(a) entry\nedge v0 v1 eps"))
;(apply-tokenizer-maker make-tokenizer "node v0 meth(main) entry\nnode v3 meth(a) entry\nedge v0 v1 eps")
;(apply-tokenizer-maker make-tokenizer "node v0 meth(main) entry")