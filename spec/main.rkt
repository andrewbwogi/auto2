#lang br/quicklang
(require "parser.rkt" "tokenizer.rkt")

(module+ reader
  (provide read-syntax))

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (strip-bindings
   #`(module spec-mod spec/main
       #,parse-tree)))

(define-macro (spec-program LINE ...)
  #'(void LINE ...))

(define-macro (spec-line A B C)
  #'(void A C (make-transition A B C)))

(define-macro (make-transition A B C)
  #'(begin
      (set-dfa-struct-transitions! dfa (cons `(,(string->symbol (second 'A))
                                               ,(string->symbol (second 'C))
                                               ,(string->symbol (second 'B)))
                                             (dfa-struct-transitions dfa)))
      (set-dfa-struct-alphabet! dfa (cons (string->symbol (second 'B))
                                         (dfa-struct-alphabet dfa)))))

(define-macro (spec-start S)
  #'(begin
       (set-dfa-struct-start! dfa (string->symbol S))
       (set-dfa-struct-states! dfa (cons (string->symbol S)
                                         (dfa-struct-states dfa)))))

(define-macro (spec-state S)
  #'(set-dfa-struct-states! dfa (cons (string->symbol S)
                                      (dfa-struct-states dfa))))

(define-macro (spec-end S)
  #'(set-dfa-struct-end! dfa (cons (string->symbol S) (dfa-struct-end dfa))))

(struct dfa-struct (alphabet states transitions start end) #:mutable #:transparent)
(define dfa (dfa-struct '() '() '() '() '()))

(define (get-dfa) (dfa-struct (remove-duplicates (dfa-struct-alphabet dfa))
                              (remove-duplicates (dfa-struct-states dfa))
                              (remove-duplicates (dfa-struct-transitions dfa))
                              (dfa-struct-start dfa)
                              (remove-duplicates (dfa-struct-end dfa))))

(provide #%module-begin get-dfa spec-end spec-state spec-start spec-line spec-program)