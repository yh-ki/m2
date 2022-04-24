#lang racket
(require "ast.rkt")
(provide intern)

;; Expr -> Expr
(define (intern e)
  ;; TODO
  e
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
  (match l
    ['() e]
    [(cons s l) (contain-str e s)])
  )

;(define (contain-str e l)
;  (match e
;    [(Int i) #f]
;    [(Bool b) #f]
;    [(Char c) #f]
;    [(Eof) #f]
;    [(Empty) #f]
;    [(Var x) #f]
;    [(Str s) (if (eq? s l) (Let (string-symbol s) (Str s) (Var (string-symbol s))) #f)]
;    [(Prim0 p) #f]
;    [(Prim1 p e)
;     (let ((x (contain-str e l)))
;       (if x (Prim1 p x) (Prim1 p e)))]
;    [(Prim2 p e1 e2)
;     (let ((x (contain-str e1 l)))
;       (if x (Prim2 p x e2) (Prim2 p e1 (contain-str e2 l))))]
;    [(Prim3 p e1 e2 e3)
;     (let ((x (contain-str e1 l)))
;       (if x (Prim3 p x e2 e3)
;           (let ((y (contain-str e2 l)))
;             (if y (Prim3 p e1 y e3) (Prim3 p e1 e2 (contain-str e3 l))))))]
;    [(If e1 e2 e3)
;     (let ((x (contain-str e1 l)))
;       (if x (If x e2 e3)
;           (let ((y (contain-str e2 l)))
;             (if y (If e1 y e3) (If e1 e2 (contain-str e3 l))))))]
;    [(Begin e1 e2)
;     (let ((x (contain-str e1 l)))
;       (if x (Begin x e2) (Begin e1 (contain-str e2 l))))]
;    [(Let x e1 e2)
;     (let ((y (contain-str e1 l)))
;       (if y (Let x y e2) (Let x e1 (contain-str e2 l))))]
;  ))