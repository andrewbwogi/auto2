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
  (define id-symbol (string->symbol id))
  (define method-symbol (string->symbol method))
  (set-cfg-struct-nodes! cfg (cons id-symbol (cfg-struct-nodes cfg)))
  (set-cfg-struct-methods! cfg (cons method-symbol (cfg-struct-methods cfg)))
  (cond [(eq? type "entry")
        (set-cfg-struct-entry-nodes! cfg (cons (node-struct id-symbol method-symbol)
                                               (cfg-struct-entry-nodes cfg)))]
        [(eq? type "ret")
        (set-cfg-struct-return-nodes! cfg (cons (node-struct id-symbol method-symbol)
                                                (cfg-struct-return-nodes cfg)))]))

(define (edge ignore id1 id2 type)
  (define id1-symbol (string->symbol id1))
  (define id2-symbol (string->symbol id2))
  (define type-symbol (string->symbol type))
  (set-cfg-struct-methods! cfg (cons type-symbol (cfg-struct-methods cfg)))
  (if (eq? type "eps")
      (set-cfg-struct-transfer-edges! cfg (cons (transfer-edge-struct id1-symbol id2-symbol)
                                                (cfg-struct-transfer-edges cfg)))
      (set-cfg-struct-call-edges! cfg (cons (call-edge-struct id1-symbol id2-symbol type-symbol)
                                            (cfg-struct-call-edges cfg)))))

(struct cfg-struct (nodes methods entry-nodes return-nodes transfer-edges call-edges) #:mutable #:transparent)
(struct node-struct (node method) #:mutable #:transparent)
(struct call-edge-struct (node1 node2 name) #:mutable #:transparent)
(struct transfer-edge-struct (node1 node2) #:mutable #:transparent)

(define cfg (cfg-struct '() '() '() '() '() '()))

(define (get-cfg)
  (set-cfg-struct-methods! cfg (remove-duplicates (cfg-struct-methods cfg)))
  cfg)

(provide #%module-begin get-cfg edge node sys-line sys-program (struct-out cfg-struct) (struct-out node-struct) (struct-out call-edge-struct) (struct-out transfer-edge-struct))