#lang br/quicklang

(module+ reader
  (provide read-syntax))

(define (read-syntax path port)
  (define (redo string)
    (string-replace
     (string-replace string #rx"\\[|\\]" "?")
     #rx"\\(|\\)"
     "!"))
  (define spec-datums
    (for/list ([spec-str (in-lines port)])
      (format-datum '(spec ~a) (redo spec-str))))
  (strip-bindings
   #`(module spec-mod spec/main
       #,@spec-datums)))

(provide #%module-begin)

(define-macro-cases spec
  [(=>!S1!-NAME->!S2!)
   #'(begin
       (set-dfa-struct-start! dfa S1)
       (set-dfa-struct-states! dfa (append '(S1 S2) (dfa-struct-states dfa)))
       (set-dfa-struct-end! dfa (append '(S1 S2) (dfa-struct-end dfa)))
       (set-dfa-struct-transitions! dfa (cons '(S1 S2 NAME) (dfa-struct-transitions dfa))))]
  [((S1)-NAME->(S2))
   #'(begin
       (set-dfa-states! dfa (append '(S1 S2) (dfa-states dfa)))
       (set-dfa-end! dfa (append '(S1 S2) (dfa-end dfa)))
       (set-dfa-transitions! dfa (cons '(S1 S2 NAME) (dfa-transitions dfa))))]
  [([S1]-NAME->(S2))
   #'(begin
       (set-dfa-states! dfa (append '(S1 S2) (dfa-states dfa)))
       (set-dfa-end! dfa (cons S2 (dfa-end dfa)))
       (set-dfa-transitions! dfa (cons '(S1 S2 NAME) (dfa-transitions dfa))))]
  [((S1)-NAME->[S2])
   #'(begin
       (set-dfa-states! dfa (append '(S1 S2) (dfa-states dfa)))
       (set-dfa-end! dfa (cons S1 (dfa-end dfa)))
       (set-dfa-transitions! dfa (cons '(S1 S2 NAME) (dfa-transitions dfa))))]
  [([S1]-NAME->[S2])
   #'(begin
       (set-dfa-states! dfa (append '(S1 S2) (dfa-states dfa)))
       (set-dfa-transitions! dfa (cons '(S1 S2 NAME) (dfa-transitions dfa))))]
  [else #'(println dfa)])
(provide spec)

(struct dfa-struct (alphabet states transitions start end) #:mutable #:transparent)
(define dfa (dfa-struct '() '() '() '() '()))

(define-macro (define/display (ID) BODY)
  #'(begin
      (define (ID) BODY)
      (module+ main
        (displayln (format "~a: ~a" 'ID (ID))))))

(define-macro (define-16bit ID PROC-ID)
  #'(define ID (compose1 mod-16bit PROC-ID)))

(define get-dfa
  dfa)

(println 'test)
(provide get-dfa)