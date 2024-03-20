#lang typed/racket
(require typed/rackunit)

; TODO:
;    - Cleanup/make prettier

; This project is fully finished


(define VALID_OPERATORS : (Listof Symbol) '(+ - * / ifleq0? func))
; The OAZO3 Language:
(struct numC ([n : Real]) #:transparent)
(struct binop ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct idC ([s : Symbol]) #:transparent) ; identifier - how we refer to variables
(struct appC ([fun : Symbol] [arg : ExprC]) #:transparent) ; Function application
(struct FunDefC ([name : Symbol] [arg : Symbol] [body : ExprC]) #:transparent) 
(struct ifleq0 ([e : ExprC] [then : ExprC] [else : ExprC]) #:transparent)

(define-type ExprC (U numC idC appC binop ifleq0))

; This function takes a s-expression and parses it into an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(list 'ifleq0? expr then else) (ifleq0 (parse expr) (parse then) (parse else))]
    [(? real? n) (numC n)]
    [(? symbol? s)
     (if (not (member s VALID_OPERATORS))
         (idC s)
         (error 'parse "OAZO: Malformed expression: ~a" s))]
    [(list (? symbol? op) l r)
     (if (or (member l VALID_OPERATORS) (member r VALID_OPERATORS) (not (member op VALID_OPERATORS)))
         (error 'parse "OAZO: Malformed operation: ~a" s)
         (binop op (parse l) (parse r)))]
    [(list (? symbol? sym) arg)
     (if (member sym VALID_OPERATORS)
         (error 'parse "OAZO: Malformed operation: ~a" s)
         (appC sym (parse arg)))] 
    [else (error 'parse "OAZO: Malformed expression: ~a" s)]))

; Testing ifleq0
(check-equal? (parse '{ifleq0? 1 0 1}) (ifleq0 (numC 1) (numC 0) (numC 1)))
(check-equal? (parse '{ifleq0? {- 2 3} 0 1}) (ifleq0 (binop '- (numC 2) (numC 3)) (numC 0) (numC 1)))

; Testing basic number parsing
(check-equal? (parse 5) (numC 5))

; Testing basic arithmetic operations
(check-equal? (parse '{+ 2 3}) (binop '+ (numC 2) (numC 3)))
(check-equal? (parse '{* 4 5}) (binop '* (numC 4) (numC 5)))
(check-equal? (parse '{- 7 2}) (binop '- (numC 7) (numC 2)))
(check-equal? (parse '{/ 9 3}) (binop '/ (numC 9) (numC 3)))

; Testing nested expressions
(check-equal? (parse '{+ {* 2 3} {- 4 1}}) 
              (binop '+ (binop '* (numC 2) (numC 3)) (binop '- (numC 4) (numC 1))))

; Test cases for malformed expressions
(check-exn (regexp (regexp-quote "OAZO: Malformed expression: (/)"))
           (lambda () (parse '{/})))

(check-exn (regexp (regexp-quote "OAZO: Malformed expression: (- 3 2 1)"))
           (lambda () (parse '{- 3 2 1})))

(check-exn (regexp (regexp-quote "OAZO: Malformed operation: (+ / 3)"))
           (lambda () (parse '{+ / 3})))

(check-exn (regexp (regexp-quote "OAZO: Malformed expression: ifleq0?"))
           (lambda () (parse 'ifleq0?)))

(check-exn (regexp (regexp-quote "OAZO: Malformed operation: (+ func 3)"))
           (lambda () (parse '{+ func 3})))

(check-exn (regexp (regexp-quote "OAZO: Malformed operation: (a b c)"))
           (lambda () (parse '{a b c})))

(check-exn (regexp (regexp-quote "OAZO: Malformed operation: (+ 0)"))
           (lambda () (parse '{+ 0})))

; Testing identifier parsing
(check-equal? (parse 'x) (idC 'x))

; Testing function application and definition parsing
(check-equal? (parse '{foo 42}) (appC 'foo (numC 42)))

; Parsing function definitions takes s expression and returns FunDefC
(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
    [(list 'func (list name arg) ': body)
     (if (and (symbol? name) (symbol? arg)
         (not (member name VALID_OPERATORS)) (not (member arg VALID_OPERATORS)))
         (FunDefC name arg (parse body))
         (error 'parse-fundef "OAZO: Expected name and arg to be non-operator symbols: ~a ~a" name arg))]
    [else (error 'parse-fundef "OAZO: Invalid function definition: ~a" s)]))

; Testing parse-fundef with incorrect name and arg types
(check-exn (regexp (regexp-quote "OAZO: Expected name and arg to be non-operator symbols"))
           (lambda () (parse-fundef '{func {42 true} : {+ x 1}})))

; Testing parse-fundef with malformed function definition (not matching expected pattern)
(check-exn (regexp (regexp-quote "OAZO: Invalid function definition"))
           (lambda () (parse-fundef '{func x y : {+ x 1}})))

; Test for malformed function def
(check-exn (regexp (regexp-quote "OAZO: Expected name and arg to be non-operator symbols"))
           (lambda () (parse-fundef '{func {+ x} : 13})))

; Parse the top level list of a program and return a list of function definitions
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (match s
    ['() '()]
    [(cons h r) (cons (parse-fundef h) (parse-prog r))]
    [else (error 'parse-prog "OAZO: Malformed program: ~a" s)]))

; Testing parse-prog with a valid list of function definitions
(check-equal? (parse-prog '{ {func {f x} : {+ x 1}} {func {g y} : {* y 2}} })
              (list (FunDefC 'f 'x (parse '{+ x 1})) 
                    (FunDefC 'g 'y (parse '{* y 2}))))

; Testing parse-prog with an empty list (valid case)
(check-equal? (parse-prog '{}) '())

; Testing parse-prog with a non-list input (malformed program)
(check-exn (regexp (regexp-quote "OAZO: Malformed program"))
           (lambda () (parse-prog 'func)))

; Substitute argument value into the function body using eager evaluation
(define (subst [body : ExprC] [arg : Symbol] [value : ExprC]) : ExprC
  (match body
    [(numC _) body]
    [(appC fun fun-arg) (appC fun (subst fun-arg arg value))]
    [(idC s) (if (eq? s arg) value body)]
    [(binop op l r) (binop op (subst l arg value) (subst r arg value))]
    [(ifleq0 test then else)
     (ifleq0 (subst test arg value)
             (subst then arg value)
             (subst else arg value))]))

; Testing subst with ifleq0 where test, then, and else branches are correctly substituted
(define testExpr (ifleq0 (idC 'x) (numC 1) (numC 2)))
(define substitutedExpr (subst testExpr 'x (numC 0)))
(check-equal? substitutedExpr (ifleq0 (numC 0) (numC 1) (numC 2)))

; Testing subst with idC where body does not match arg
(check-equal? (subst (idC 'y) 'x (numC 42)) (idC 'y))


; Takes an expression and a list of function definitions and outputs a real number
; by evaluating the expressions and using subsitution within functions.
(define (interp [e : ExprC] [fds : (Listof FunDefC)]) : Real
  (match e
    [(appC fun args)
     (match (find-func fun fds)
       [(FunDefC n arg body) (interp (subst body arg (numC (interp args fds))) fds)]
    )]
    [(numC n) n]
    [(binop '+ l r) (+ (interp l fds) (interp r fds))]
    [(binop '- l r) (- (interp l fds) (interp r fds))]
    [(binop '* l r) (* (interp l fds) (interp r fds))]
    [(binop '/ l r) 
     (let ([denom (interp r fds)])
         (if (= denom 0)
             (error 'interp "OAZO: Attempted division by zero")
             (/ (interp l fds) denom)))]
    [(ifleq0 exp then els)
     (if (<= (interp exp fds) 0)
         (interp then fds)
         (interp els fds))]))

; Testing arithmetic operations
(check-equal? (interp (binop '+ (numC 2) (numC 3)) '()) 5)
(check-equal? (interp (binop '* (numC 4) (numC 5)) '()) 20)
(check-equal? (interp (binop '- (numC 7) (numC 2)) '()) 5)
(check-equal? (interp (binop '/ (numC 9) (numC 3)) '()) 3)

; Testing ifleq0 expressions
(check-equal? (interp (ifleq0 (numC 1) (numC 0) (numC 1)) '()) 1) ; 1 > 0, so result should be 1
(check-equal? (interp (ifleq0 (binop '- (numC 2) (numC 3)) (numC 0) (numC 1)) '()) 0) ; 2-3 <= 0, so result should be 0

; Testing nested expressions
(check-equal? (interp (binop '+ (binop '* (numC 2) (numC 3)) (binop '- (numC 4) (numC 1))) '()) 9)

; Testing interp division by 0 handling
(check-exn (regexp (regexp-quote "OAZO: Attempted division by zero"))
           (lambda () (interp (binop '/ (numC 9) (numC 0)) '())))


; Function to pull the main function from the definitions and interp it
(define (interp-fns [funs : (Listof FunDefC)]) : Real
  (let ([main-fn (find-func 'main funs)])
    ; As stated in class, main will not take
    ; an actual arg, just a dummy for now
    (interp (appC 'main (numC 0)) funs)))

; Finds a function by it's name (symbol)
(define (find-func [name : Symbol] [funs : (Listof FunDefC)]) : FunDefC
  (match funs
    ['() (error 'find-func "OAZO: ~a function not found" name)]
    [(cons first-fun rest-funs)
     (if (eq? (FunDefC-name first-fun) name)
         first-fun
         (find-func name rest-funs))]))


; Define some sample function definitions
(define main-fun (FunDefC 'main 'x (numC 42)))
(define other-fun (FunDefC 'other 'y (numC 100))) 

; Testing interp-fns with a valid main function
(check-equal? (interp-fns (list main-fun other-fun)) 42)

; Testing interp-fns when main function is absent
(check-exn (regexp (regexp-quote "OAZO: main function not found"))
           (lambda () (interp-fns (list other-fun))))

; Testing find-func to correctly find a function
(check-equal? (find-func 'main (list main-fun other-fun)) main-fun)

; Testing find-func when the function is absent
(check-exn (regexp (regexp-quote "OAZO: some-func function not found"))
           (lambda () (find-func 'some-func (list main-fun other-fun))))

; Testing find-func with an empty list of functions
(check-exn (regexp (regexp-quote "OAZO: main function not found"))
           (lambda () (find-func 'main '())))

; top-interp takes a s-epression and combines the parser and interperater that we made
; to produce a real number
(define (top-interp [fun-sexps : Sexp]) : Real
  (interp-fns (parse-prog fun-sexps)))

; top-interp test cases
(check-equal? (top-interp '{{func {f x} : {+ x 14}}
                             {func {main init} : {f 2}}})
              16)

(check-equal? (top-interp '{{func {f x} : {* {+ x 14} 2}}
                             {func {main init} : {f 2}}})
              32)

(check-equal? (top-interp '{{func {f x} : {/ {+ x 14} 2}}
                             {func {main init} : {f 2}}})
              8)

; Testing function application with nested calls
(check-equal? (top-interp '{{func {t x} : {+ x {* {f 5} {f 5}}}}
                             {func {f x} : {+ x 14}}
                             {func {main init} : {t 2}}})
              363)

(check-equal? (top-interp '{{func {g x} : {+ x 1}}
                           {func {f x} : {g {g x}}}
                           {func {main init} : {f 5}}})
              7)

; Testing function application with multiple arguments
(check-exn (regexp (regexp-quote "Invalid function definition"))
           (lambda () (top-interp '{{func {f x y} : {+ x y}}
                                   {func {main init} : {f 2 3}}})))

; Testing top-interp with division by zero within a function call
(check-exn (regexp (regexp-quote "OAZO: Attempted division by zero"))
           (lambda () (top-interp '((func (ignoreit x) : (+ 3 4)) (func (main init) : (ignoreit (/ 1 (+ 0 0))))))))

; Testing error handling for undefined functions
(check-exn (regexp (regexp-quote "OAZO: undefined-func function not found"))
           (lambda () (top-interp '{{func {main init} : {undefined-func 5}}})))

; Testing error handling for malformed ifleq0 expressions
(check-exn (regexp (regexp-quote "no matching clause for "))
           (lambda () (top-interp '{{func {main init} : {ifleq0? {+ 1 2} {+ 3 4}}}})))

; Ensure expressions involving unbound identifiers trigger an error
(check-exn (regexp (regexp-quote "OAZO: Malformed expression"))
           (lambda () (top-interp '{{func {main init} : {unbound-identifier}}})))

; Test error handling for incorrect number of elements in function application
(check-exn (regexp (regexp-quote "Malformed expression"))
           (lambda () (top-interp '{{func {f x} : {+ x 10}}
                                   {func {main init} : {f}}})))
