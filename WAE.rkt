#lang plai

;type definition of WAE

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [id (name symbol?)]
  [with (name symbol?)(named-expr WAE?) (body WAE?)]
  )


;parse-wae

(define (parse-wae sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+) (add (parse-wae (second sexp))
                 (parse-wae (third sexp)))]
       [(-) (sub (parse-wae (second sexp))
                 (parse-wae (third sexp)))]
       [(with) (with (first (second sexp)) 
                    (parse-wae (second (second sexp))) 
                    (parse-wae (third sexp)))])]
    [else (error parse-wae "Parse error")]
       ))



;basic substitute

(define (subst expr sub-id val)
  (type-case WAE expr
    [num (n) expr]
    [id (v) (if (symbol=? v sub-id) val expr)]
    [add (l r) (add (subst l sub-id val) (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val) (subst r sub-id val))]
    [with (bound-id named-expr bound-body)
      (if (symbol=? bound-id sub-id)
        (with bound-id
          (subst named-expr sub-id val) bound-body)
        (with bound-id
          (subst named-expr sub-id val)
          (subst bound-body sub-id val)))]
    ))


;interp-wae

(define (interp-wae expr)
  (type-case WAE expr
    [num (n) n]
    [add (l r) (+ (interp-wae l)(interp-wae r))]
    [sub (l r) (- (interp-wae l)(interp-wae r))]
    [with (bound-id named-expr bound-body)
          (interp-wae (subst bound-body bound-id
                                  (num (interp-wae named-expr))))]
    [id (v)(error 'interp-wae "free identifier")]))

;evaluator

(define eval-wae
  (lambda (Sexp)
    (interp-wae (parse-wae Sexp))))

;Testing
(test (eval-wae '1) 1)
(test (eval-wae '(+ 3 3)) 6)
(test (eval-wae '(with (x (+ 1 2)) (+ x x))) 6)
(test (eval-wae '(with (y 7) (with (x (- 10 y)) x))) 3)