#lang br/quicklang
(require "parser.rkt" "tokenizer.rkt")

(module+ reader
  (provide read-syntax))

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (strip-bindings
   #`(module sys-mod sys/main
       #,parse-tree)))

(define-macro (sys-program LINE ...)
  #'(void LINE ...))

(define-macro (sys-line NODEOREDGE)
  #'(void NODEOREDGE))

(define (node ignore id method type)
  (if (eq? type "entry")
      (set-cfg-struct-entry-nodes! cfg (cons (node-struct id method)
                                             (cfg-struct-entry-nodes cfg)))
      (set-cfg-struct-return-nodes! cfg (cons (node-struct id method)
                                              (cfg-struct-return-nodes cfg)))))

(define (edge ignore id1 id2 type)
  (if (eq? type "eps")
      (set-cfg-struct-transfer-edges! cfg (cons (transfer-edge-struct id1 id2)
                                                (cfg-struct-transfer-edges cfg)))
      (set-cfg-struct-call-edges! cfg (cons (call-edge-struct id1 id2 type)
                                            (cfg-struct-call-edges cfg)))))

(struct cfg-struct (entry-nodes return-nodes transfer-edges call-edges) #:mutable #:transparent)
(struct node-struct (node method) #:mutable #:transparent)
(struct call-edge-struct (node node2 name) #:mutable #:transparent)
(struct transfer-edge-struct (node1 node2) #:mutable #:transparent)

(define cfg (cfg-struct '() '() '() '()))

(define (get-cfg) cfg)

(provide #%module-begin get-cfg edge node sys-line sys-program)