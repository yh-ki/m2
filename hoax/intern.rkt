#lang racket
(require "ast.rkt")
(provide intern)

;; Expr -> Expr
(define (intern e)
  ;; TODO
  (make-e (var-str e) (count-str e '()))
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

(define (var-str e)
  (match e
    [(Int i) (Int i)]
    [(Bool b) (Bool b)]
    [(Char c) (Char c)]
    [(Eof) (Eof)]
    [(Empty) (Empty)]
    [(Var x) (Var x)]
    [(Str s) (Var (string->symbol s))]
    [(Prim0 p) (Prim0 p)]
    [(Prim1 p e) (Prim1 p (var-str e)) ]
    [(Prim2 p e1 e2) (Prim2 p (var-str e1) (var-str e2))]
    [(Prim3 p e1 e2 e3) (Prim3 p (var-str e1) (var-str e2) (var-str e3))]
    [(If e1 e2 e3) (If (var-str e1) (var-str e2) (var-str e3))]
    [(Begin e1 e2) (Begin (var-str e1) (var-str e2))]
    [(Let x e1 e2) (Let x (var-str e1) (var-str e2))]
    )
  )

(define (make-e e l)
  (match l
    ['() e]
    [(cons s l) (make-e (Let (string->symbol s) (Str s) e) l)]))

;(define (replace-str e l)
;  (match l
;    ['() e]
;    [(cons s l) (contain-str e s)])
;  )

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