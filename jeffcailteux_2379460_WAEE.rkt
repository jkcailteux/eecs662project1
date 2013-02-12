#lang plai

;type definition of WAEE

(define-type WAEE
  [num (n number?)]
  [op (oper symbol?) (lhs WAEE?) (rhs WAEE?)]
  [id (name symbol?)]
  [with (name symbol?)(named-expr WAEE?) (body WAEE?)]
  )

;operators
;list of lists of string operator pairs
(define binop
  (list (list 'add +)
        (list 'sub -)
        (list 'mul *)
        (list 'div /)
        ))

;get operator
;turns operator symbols into operator text by looking up binop type
(define get-binop
  (lambda (type binop)
    (cond ((empty? binop)
           (error 'get-binop "Bad Operator"))
          (else (if (symbol=? type (first (first binop))) 
                    (cadar binop)(get-binop type (cdr binop)))
                )
          )))

;binding check
(define binding?
  (lambda (expr)
    (and (symbol? (first expr)) 
         (WAEE? (second expr)))))
       
;list of binding check
(define lob?
  (lambda (expr)
    (cond ((empty? expr) #t)
          (else (and (binding? (car expr)) 
                     (lob? (cdr expr)))))))

;Parser
;takes expression and turns into WAEE structure
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
       [(with) (expand-with (second sexp)(third sexp))]
       )
     ]
     [else (error 'parse-waee "Parse error")]
     )
  )

;interpreter
;turn WAEE structure into scheme and executes
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
;takes an expression, replaces instances of sub-id with val
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
;takes string starting after with
;checks if single binding or multiple
;if single, return basic with
;if multiple, go through list of bindings and send to parser
;one binding at a time with expr
(define (expand-with binding-instance expr)
  (cond
    ((empty? binding-instance) (parse-waee expr))
    ;for single binding instance
    ((symbol? (first binding-instance))
     (with (first binding-instance)
           (parse-waee (second binding-instance))(parse-waee expr))
     )
    ;for multiple binding instances
    (else (with (first (first binding-instance)) 
                (parse-waee (cadar binding-instance)) 
                (expand-with (cdr binding-instance) expr))
          ))
  )  

;evaluator
;take expression, give to parse, take WAEE structure and give to interpreter
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
(test (eval-waee '(with ((x 5) (y 5)) (+ x y))) 10)
(test (eval-waee '(with ((x 5) (y 5) (z 5)) (+ (+ x y) z))) 15)
