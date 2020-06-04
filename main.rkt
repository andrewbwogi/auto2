#lang racket
(require "constants.rkt" "spec/main.rkt" "sys/main.rkt" rackunit)
;(require "autotest/Vote/Vote-cfg.rkt" "autotest/Vote/Vote_gv-spec.rkt")
;(require "autotest/Vote/Vote-cfg.rkt" "autotest/Vote/Vote_v-spec.rkt")
;(require "autotest/Simple/simple.rkt" "autotest/Simple/simple-cfg.rkt")
;(require "autotest/Simple/simple-sys-true.rkt" "autotest/Simple/simple-cfg.rkt")
;(require "autotest/Vote/Vote_ne-cfg.rkt" "autotest/Vote/Vote_v_ne-spec.rkt")
(require "autotest/Vote/Vote_ne-cfg.rkt" "autotest/Vote/Vote_gv_ne-spec.rkt")
;(require "autotest/EvenOdd/EvenOdd-cfg.rkt" "autotest/EvenOdd/EvenOdd1a-spec.rkt")
;(require "autotest/EvenOdd/EvenOdd-cfg.rkt" "autotest/EvenOdd/EvenOdd1b-spec.rkt")
;(require "autotest/EvenOdd/EvenOdd-cfg.rkt" "autotest/EvenOdd/EvenOdd2a-spec.rkt")
;(require "autotest/EvenOdd/EvenOdd-cfg.rkt" "autotest/EvenOdd/EvenOdd2b-spec.rkt")
;(require "autotest/EvenOdd/EvenOdd-cfg.rkt" "autotest/EvenOdd/EvenOdd3a-spec.rkt")
;(require "autotest/EvenOdd/EvenOdd-cfg.rkt" "autotest/EvenOdd/EvenOdd3b-spec.rkt")
;(require "autotest/Simplest/simplest.rkt" "autotest/Simplest/simplest-cfg.rkt")
(define dfa-in (get-dfa))
(define cfg-in (get-cfg))
(define cfg-prod (cfg-prod-struct '() '() '() '()))
(define start-boxes '())

; terminals
(define (create-terminals dfa-in)
  (dfa-struct-alphabet dfa-in))

; non terminals
(define (create-non-terminal dfa-in cfg-in)
  (cons 's 
        (for*/list ([s1 (dfa-struct-states dfa-in)]
                    [s2 (dfa-struct-states dfa-in)]
                    [x (append (cfg-struct-nodes cfg-in)
                               (dfa-struct-alphabet dfa-in))])
          (list s1 x s2))))

; production rule 1
(define (find-main-node cfg-in)
  (for/first ([n (cfg-struct-entry-nodes cfg-in)]
              #:when (string-contains? (symbol->string (node-struct-method n)) "main"))
    (node-struct-node n)))

(define (p1 dfa-in cfg-in)
  (define m (find-main-node cfg-in))
  (for*/list ([qi (dfa-struct-states dfa-in)]
              #:when (not (member qi (dfa-struct-end dfa-in))))
    (define b (box 1))
    (set! start-boxes (cons b start-boxes))
    (production 's (list (dfa-struct-start dfa-in) m qi) b 0)))

; production rule 2
(define (p2 dfa-in cfg-in)
  (for*/list ([s1 (dfa-struct-states dfa-in)]
              [s2 (dfa-struct-states dfa-in)]
              [t (cfg-struct-transfer-edges cfg-in)])
    (production (list s1 (transfer-edge-struct-node1 t) s2)
                (list s1 (transfer-edge-struct-node2 t) s2)
                (box 1) 0)))

; production rule 2b
; if DFA is underspecified and does not say how a method call should
; be interpreted, the method call is treated as a silent call
(define (p2b dfa-in cfg-in)
  (define ext (for/list ([t (cfg-struct-methods cfg-in)]
                         #:when (not (member t (dfa-struct-alphabet dfa-in))))
                t))
  (for*/list ([s1 (dfa-struct-states dfa-in)]
              [s2 (dfa-struct-states dfa-in)]
              [t (cfg-struct-call-edges cfg-in)]
              #:when (member (call-edge-struct-name t) ext))
    (production (list s1 (call-edge-struct-node1 t) s2)
                (list s1 (call-edge-struct-node2 t) s2)
                (box 1) 0)))

; production rule 3
(define (find-entry-node name)
  (for/first ([e (cfg-struct-entry-nodes cfg-in)]
              #:when (eq? name (node-struct-method e)))
    (node-struct-node e)))

(define (p3 dfa-in cfg-in)
  (for*/list ([s1 (dfa-struct-states dfa-in)]
              [s2 (dfa-struct-states dfa-in)]
              [s3 (dfa-struct-states dfa-in)]
              [s4 (dfa-struct-states dfa-in)]
              [t (cfg-struct-call-edges cfg-in)])
    (production (list s1 (call-edge-struct-node1 t) s4)
                (list (list s1 (call-edge-struct-name t) s2)
                      (list s2 (find-entry-node (call-edge-struct-name t)) s3)
                      (list s3 (call-edge-struct-node2 t) s4))
                (box 3) 0)))

; production rule 4
(define (p4 dfa-in cfg-in)
  (for*/list ([s1 (dfa-struct-states dfa-in)]
              [r (cfg-struct-return-nodes cfg-in)])
    (production (list s1 (node-struct-node r) s1) 'eps (box 0) 0)))

; production rule 5
(define (p5 dfa-in)
  (for/list ([t (dfa-struct-transitions dfa-in)])
    (production (list (first t) (third t) (second t)) (third t) (box 0) 0)))

(define (create-cfg dfa-in cfg-in)
  (cfg-prod-struct (create-non-terminal dfa-in cfg-in)
                   (create-terminals dfa-in)
                   (append (p1 dfa-in cfg-in) (p2 dfa-in cfg-in)
                           (p3 dfa-in cfg-in) (p4 dfa-in cfg-in)
                           (p5 dfa-in) (p2b dfa-in cfg-in))
                   's))

; simplify cfg
(define unit 0)
(define unit-table (make-hash))

(define (get-unit symbol)
  (if (hash-has-key? unit-table symbol)
      (hash-ref unit-table symbol)
      (begin
        (set! unit (+ unit 1))
        (hash-set! unit-table symbol unit)
        unit)))

(define (unit-non-terminals cfg-prod)
  (for/list ([t (cfg-prod-struct-non-terminals cfg-prod)])
    (get-unit t)))

(define terminal-hash (make-hash))

(define (unit-terminals cfg-prod)
  (for/list ([t (cfg-prod-struct-terminals cfg-prod)])
    (define u (get-unit t))
    (hash-set! terminal-hash u t)
    u))

(define (create-new-triple to)
  (list (get-unit (first to))
        (get-unit (second to))
        (get-unit (third to))))

(define (unit-productions cfg-prod)
  (for/list ([t (cfg-prod-struct-productions cfg-prod)])
    (define from (production-from t))
    (define to (production-to t))
    (define new-to '())
    (define new-from (get-unit from))
    (if (and (list? to) (list? (first to)))
        (set! new-to (create-new-triple to))
        (set! new-to (get-unit to)))
    (define f (if (list? new-to) new-to (list new-to)))
    ; 0. a common full body
    (production new-from new-to (production-counter t) (box f))))

(define (unit-start cfg-prod) (get-unit (cfg-prod-struct-start cfg-prod)))

(define (create-simple-cfg)
  (define cfg (create-cfg dfa-in cfg-in))
  (cfg-prod-struct (unit-non-terminals cfg)
                   (unit-terminals cfg)
                   (unit-productions cfg)
                   (unit-start cfg)))

; test for language emptiness
(define (create-vector cfg unit)
  (define len (+ unit 1))
  (define generating? (make-vector len #f))
  (define list-vec (make-vector len '()))
  (for ([s (in-range 1 len 1)])
    (define new-list
      (for/fold ([l '()])
                ([p (cfg-prod-struct-productions cfg)])
        (append l
                (if (list? (production-to p))
                    (add-triple-link p s cfg)
                    (add-single-link p s cfg)))))
    (vector-set! list-vec s new-list))
  (values generating? list-vec))

(define (add-triple-link p s cfg)
  (for/list ([e (production-to p)]
             #:when (and (eq? e s)
                         (not (member e (cfg-prod-struct-terminals cfg)))))
    ; 0. a common full body
    (link (production-from p) e (production-counter p) (production-full p)))) 

(define (add-single-link p s cfg)
  (if (and (eq? (production-to p) s)
           (not (member (production-to p) (cfg-prod-struct-terminals cfg))))
      ; 0. a common full body
      (list (link (production-from p) (production-to p) (production-counter p) (production-full p))) 
      '()))

(define (set-non-terminals cfg gen)
  (define queue
    (for/list ([p (cfg-prod-struct-productions cfg)]
               #:when (and (not (list? (production-to p)))
                           (member (production-to p) (cfg-prod-struct-terminals cfg))))
      ; 1. add produced letter with number in queue
      (list (production-from p) (production-to p)))) 
  (values gen queue))

(define (set-terminals cfg gen)
  (for ([e (cfg-prod-struct-terminals cfg)])
    (vector-set! gen e #t)))

(define counter-example '())

(define (tick-down gen vec queue)
  (define new-q '())
  (for ([q queue]
        ; only count down once
        #:when (not (vector-ref gen (first q))))
    ; for each link in a row 
    (for ([l (vector-ref vec (first q))])
      (define c (- (unbox (link-counter l)) 1))
      (define (func current-variable full)
        ; 2. when counting down, replace variable with letter
        (lambda (n) (if (eq? n current-variable) full n))) 
      (define f (map (func (link-variable l) (second q))
           (unbox (link-full l))))
      (define final-f (if (list? f) (flatten f) f))
      (set-box! (link-counter l) c)
      (set-box! (link-full l) final-f)
      (when (eq? (link-head l) 1) (set! counter-example final-f))
      (when (eq? c 0)
        ; 1. add produced letter with number in queue
        (set! new-q (cons (list (link-head l) (unbox (link-full l))) new-q)))) 
    (vector-set! gen (first q) #t))
  (values gen new-q))

(define (tick-down-all gen vec queue)
  (define-values (g q) (tick-down gen vec queue))
  (if (empty? q)
      gen
      (tick-down-all g vec q)))

(define (is-generating)
  (define cfg (create-simple-cfg))
  (define-values (g l) (create-vector cfg unit))
  (set-terminals cfg g)
  (define-values (g2 q) (set-non-terminals cfg g))
  (define g3 (tick-down-all g2 l q))
  (define generating (vector-ref g3 (cfg-prod-struct-start cfg)))
  (when generating (println (map (lambda (n) (hash-ref terminal-hash n)) counter-example)))
  generating)

