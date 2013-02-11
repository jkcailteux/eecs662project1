#lang plai

;type definition of WAEE

(define-type WAEE
  [num (n number?)]
  [op (oper symbol?) (lhs WAEE?) (rhs WAEE?)]
  [id (name symbol?)]
  [with (name symbol?)(named-expr WAEE?) (body WAEE?)]
  )

;operators
(define binop
  (list (list 'add +)
        (list 'sub -)
        (list 'mul *)
        (list 'div /)
        ))

;get operator
(define get-binop
  (lambda (type binop)
    (cond ((empty? binop)(error 'get-binop "Bad Operator"))
          (else (if (symbol=? type (first (first binop))) (cadar binop)
                    (get-binop type (cdr binop)))))))

;Parser
(define (parse-waee sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+) (op 'add (parse-waee (second sexp))
                 (parse-waee (third sexp)))]
       [(-) (op 'sub (parse-waee (second sexp))
                 (parse-waee (third sexp)))]
       [(*) (op 'mul (parse-waee (second sexp))
                 (parse-waee (third sexp)))]
       [(/) (op 'div (parse-waee (second sexp))
                 (parse-waee (third sexp)))]
       [(with) (expand-with (second sexp)(third sexp))])]
     [else (error 'parse-waee "Parse error")]))

;interpreter
(define (interp-waee expr)
  (type-case WAEE expr
    [num (n) n]
    [op (oper l r) ((get-binop oper binop) (interp-waee l) (interp-waee r))]
    [id (v)(error 'interp-waee "free identifier")]
    [with (bound-id named-expr bound-body)
          (interp-waee (subst bound-body bound-id
                                  (num (interp-waee named-expr))))]
    ))

;substitution
(define (subst expr sub-id val)
  (type-case WAEE expr
    [num (n) expr]
    [id (v) (if (symbol=? v sub-id) val expr)]
    [op (oper l r) (op oper (subst l sub-id val) (subst r sub-id val))]
    [with (bound-id named-expr bound-body)
      (if (symbol=? bound-id sub-id)
        (with bound-id
          (subst named-expr sub-id val) bound-body)
        (with bound-id
          (subst named-expr sub-id val)
          (subst bound-body sub-id val)))]
    ))

;expand-with
(define (expand-with binding-instance expr)
  (cond
    ((empty? binding-instance) (parse-waee expr))
    ((symbol? (first binding-instance))
     (with (first binding-instance)
           (parse-waee (second binding-instance))(parse-waee expr)))
    (else (with (first (first binding-instance)) 
                (parse-waee (cadar binding-instance)) 
                (expand-with (cdr binding-instance) expr)))))  

;evaluator
(define eval-waee
  (lambda (Sexp)
    (interp-waee (parse-waee Sexp))))
    
       

;Testing
(test (eval-waee '1) 1)
(test (eval-waee '(+ 3 3)) 6)
(test (eval-waee '(* 3 3)) 9)
(test (eval-waee '(/ 3 3)) 1)
(test (eval-waee '(with (x (+ 1 2)) (+ x x))) 6)
(test (eval-waee '(with (y 7) (with (x (- 10 y)) x))) 3)
(test (eval-waee '(with (y (- 2 1)) (with (x (+ y 1))x))) 2)

