#lang br/quicklang
(require brag/support)

(define (make-tokenizer port)
  (define (next-token)
    (define sys-lexer
      (lexer
       ["node"
        (token 'NODE-TOK "node")]
       [(from/to " meth(" ")")
        (token 'METH-TOK (trim-ends " meth(" lexeme ")"))]
       [(:or "\n" " ") (next-token)]
       ["edge"
        (token 'EDGE-TOK "edge")]
       [(:+ (:or alphabetic numeric "-" "_"))
        (token 'VAL-TOK lexeme)]
       [(:or "\n" " ") (next-token)]
       ))
    (sys-lexer port))  
  next-token)
(provide make-tokenizer)