#lang racket
(require "ast.rkt")
(provide intern)

;; Expr -> Expr
(define (intern e)
  ;; TODO
  (replace-str e (count-str e '()))
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

(define (replace-str e l)
  (match e
    [(Int i) (Int i)]
    [(Bool b) (Bool b)]
    [(Char c) (Char c)]
    [(Eof) (Eof)]
    [(Empty) (Empty)]
    [(Var x) (Var x)]
    [(Str s) (if (member s l) (remove s l) (Var (string->symbol s)))]
    [(Prim0 p) l]
    [(Prim1 p e) (replace-str e l)]
    [(Prim2 p e1 e2) (replace-str e2 (replace-str e1 l))]
    [(Prim3 p e1 e2 e3) (replace-str e3 (replace-str e2 (replace-str e1 l)))]
    [(If e1 e2 e3) (replace-str e3 (replace-str e2 (replace-str e1 l)))]
    [(Begin e1 e2) (replace-str e2 (replace-str e1 l))]
    [(Let x e1 e2) (replace-str e2 (replace-str e1 l))]
    )
  )