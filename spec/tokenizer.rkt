#lang br/quicklang
(require brag/support)

(define (make-tokenizer port)
  (define (next-token)
    (define spec-lexer
      (lexer
       [(from/to "=>(" ")")
        (token 'START-TOK (trim-ends "=>(" lexeme ")"))]
       [(from/to "(" ")")
        (token 'END-TOK (trim-ends "(" lexeme ")"))]
       [(from/to "[" "]")
        (token 'STATE-TOK (trim-ends "[" lexeme "]"))]
       [(from/to "-" "->")
        (token 'NAME-TOK (trim-ends "-" lexeme "->"))]
       ["\n" (next-token)]
       [any-char (next-token)]))
    (spec-lexer port))  
  next-token)
(provide make-tokenizer)