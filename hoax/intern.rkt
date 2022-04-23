#lang racket
(require "ast.rkt")
(provide intern)

;; Expr -> Expr
(define (intern e)
  ;; TODO
  (count-str e '())
  )

(define (count-str e l)
  (match e
    [(Int i) l]
    [(Bool b) l]
    [(Char c) l]
    [(Eof) l]
    [(Empty) l]
    [(Var x) l]
    [(Str s) (if (member s l) l (cons s l))]
    [(Prim0 p) l]
    [(Prim1 p e) (count-str e l)]
    [(Prim2 p e1 e2) (count-str e1 (count-str e2 l))]
    [(Prim3 p e1 e2 e3) (count-str e1 (count-str e2 (count-str e3 l)))]
    [(If e1 e2 e3) (count-str e1 (count-str e2 (count-str e3 l)))]
    [(Begin e1 e2) (count-str e1 (count-str e2 l))]
    [(Let x e1 e2) (count-str e1 (count-str e2 l))]
    )
  )