#lang typed/racket
(require typed/rackunit)

; Progress Toward Goal comment:
; - Full project implemented.

(define RESERVED_SYMBOLS : (Listof Symbol) '(if anon then else let : <-))

; The OAZO5 Language:
(struct numC ([n : Real]) #:transparent)
(struct strC ([str : String]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct ifC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct appC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct lamC ([arg : (Listof Symbol)] [body : ExprC]) #:transparent) ; replaces our fundef

; Env stuff
(struct Bind ([name : Symbol] [val : Value]) #:transparent)
(struct Env ([l : (Listof Bind)]) #:transparent)

; Add values to the language
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StrV ([s : String]) #:transparent)
(struct ClosV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent) 
(struct PrimV ([func : (NumV NumV -> Value)]) #:transparent)
(struct PrimVAny ([func : (Any Any -> Value)]) #:transparent)
(struct ErrorV ([func : (Value -> Void)]) #:transparent)

(define-type Value (U NumV BoolV StrV ClosV PrimV PrimVAny ErrorV))
(define-type ExprC (U numC strC idC ifC lamC appC))


; This function takes a value and returns its string representation.
(define (serialize [v : Value]) : String
  (match v
    [(NumV n) (format "~v" n)]
    [(BoolV b) (if b "true" "false")]
    [(StrV s) (format "~v" s)]
    [(ClosV p b e) "#<procedure>"]
    [(PrimV f) "#<primop>"]
    [(ErrorV f) "#<error>"]))


; Primitives
; Primitive operation for add
(define (prim+ [arg1 : NumV] [arg2 : NumV]) : Value
  (NumV (+ (NumV-n arg1) (NumV-n arg2))))

; Primitive operation for subtract
(define (prim- [arg1 : NumV] [arg2 : NumV]) : Value
  (NumV (- (NumV-n arg1) (NumV-n arg2))))

; Primitive operation for multiply
(define (prim* [arg1 : NumV] [arg2 : NumV]) : Value
  (NumV (* (NumV-n arg1) (NumV-n arg2))))

; Primitive operation for divide
(define (prim/ [arg1 : NumV] [arg2 : NumV]) : Value
  (if (equal? (NumV-n arg2) 0)
      (error 'interp "OAZO: Division by zero")
      (NumV (/ (NumV-n arg1) (NumV-n arg2)))))

; Primitive operation for leq
(define (prim<= [arg1 : NumV] [arg2 : NumV]) : Value
  (BoolV (<= (NumV-n arg1) (NumV-n arg2))))

; Primitive operation for equal?
(define (prim? [arg1 : Any] [arg2 : Any]) : Value
  (if (or (PrimVAny? arg1) (PrimVAny? arg1) (PrimV? arg1) (PrimV? arg2) (ClosV? arg1) (ClosV? arg2))
    (BoolV #f)
    (BoolV (equal? arg1 arg2))))

; Tests for our primitives
(check-equal? (prim+ (NumV 5) (NumV 5)) (NumV 10))
(check-equal? (prim- (NumV 10) (NumV 5)) (NumV 5))
(check-equal? (prim* (NumV 5) (NumV 5)) (NumV 25))
(check-equal? (prim/ (NumV 10) (NumV 5)) (NumV 2))
(check-equal? (prim<= (NumV 4) (NumV 5)) (BoolV #t))
(check-equal? (prim? (NumV 5) (NumV 5)) (BoolV #t))

; Division by zero test
(check-exn (regexp "OAZO: Division by zero") (lambda () (prim/ (NumV 5) (NumV 0))))

; User error handler, takes value and raises an error
(define (error-handler [value : Value]) : Void
  (error 'interp "user-error: ~a" (serialize value)))

; Base env
(define base-env
  (Env (list (Bind '+ (PrimV prim+))
        (Bind '- (PrimV prim-))
        (Bind '* (PrimV prim*))
        (Bind '/ (PrimV prim/))
        (Bind '<= (PrimV prim<=))
        (Bind 'equal? (PrimVAny prim?))
        (Bind 'true (BoolV #t))
        (Bind 'false (BoolV #f))
        (Bind 'error (ErrorV error-handler)))))


; prim? closure test
(check-equal? (prim? (ClosV '() (numC 5) base-env) (ClosV '() (numC 5) base-env)) (BoolV #f))

; Tests for serialize
(check-equal? (serialize (NumV 34)) "34")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (StrV "hello")) "\"hello\"")
(check-equal? (serialize (ClosV '() (numC 5) base-env)) "#<procedure>")
(check-equal? (serialize (PrimV prim+)) "#<primop>")
(check-equal? (serialize (ErrorV error-handler)) "#<error>")


; This function checks to see if a list has any duplicate symbols
(define (duplicates? [items : (Listof Symbol)]) : Boolean
  (match items
    ['() #false]
    [(cons head rest)
     (or (not (eq? (member head rest) #f))
         (duplicates? rest))]))


; This fucntion checks if our body has an arrow in it, meaning its a malformed let
(define (contains-arrow? [body : Sexp]) : Boolean
  (match body
    [(list var '<- expr) #true]
    [_ #false]))
    

; This function takes a s-expression and parses it into an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (numC n)]
    [(? string? str) (strC str)]
    [(? symbol? sym)
     (if (member sym RESERVED_SYMBOLS)
         (error 'parse "OAZO: Tried to use reserved symbol: ~a" sym)
         (idC sym))]
    [(list 'if test 'then primary 'else otherwise)
     (if (or (member test RESERVED_SYMBOLS)
             (member primary RESERVED_SYMBOLS)
             (member otherwise RESERVED_SYMBOLS))
         (error 'parse "OAZO: Reserved symbol used in if expression: ~a" s)
         (ifC (parse test) (parse primary) (parse otherwise)))]
    [(list 'let args ...)
     (let ([bindings (take args (- (length args) 1))] [body (last args)])
       (if (contains-arrow? body)
           (error 'parse "OAZO: No body for let")
           (let ([parsed-bindings ((inst map (Pair Symbol ExprC) Sexp) parse-let-binding bindings)])       
             (desugar-let parsed-bindings (parse body)))))]
    [(list* 'anon params ': (list body))
     (let ([checked-params
            (if (empty? params)
                params
                (map (lambda ([param : Sexp])
                       (cond
                         [(symbol? param) param]
                         [else (error 'parse "OAZO: Invalid parameter: ~a" param)]))
                     (if (list? params)
                         params
                         (error 'parse "OAZO: Expected list of parameters, got: ~a" params))))])
       (if (duplicates? checked-params)
           (error 'parse "OAZO: Duplicate args for function: ~a" checked-params)
           (lamC checked-params (parse body))))]
    [(list fun args ...) (appC (parse fun) (map parse args))]
    [_ (error 'parse "OAZO: Unsupported expression: ~a" s)]))


; This function parses the concrete syntax of a let binding into a list of symbol and expr
(define (parse-let-binding [b : Sexp]) : (Pair Symbol ExprC)
  (match b
    [(list var '<- expr) 
      (cond 
        [(and (symbol? var) (not (member var RESERVED_SYMBOLS))) (cons var (parse expr))]
        [else (error 'parse-let-binding "OAZO: Invalid let binding: ~a" b)])]
    [_ (error 'parse-let-binding "OAZO: Invalid binding syntax: ~a" b)]))

(check-equal? (parse-let-binding '{x <- 5}) (cons 'x (numC 5)))
(check-exn (regexp "OAZO: Invalid binding syntax") (lambda () (parse-let-binding (cons 'x 5))))

; This function takes our parsed let bindings and the body of our let and turns it into an application
(define (desugar-let [bindings : (Listof (Pair Symbol ExprC))] [body : ExprC]) : ExprC
  ; Accumulator for the vars for the final lambda
  (let desugar ([bs bindings] [vars : (Listof Symbol) '()] [exprs : (Listof ExprC) '()])
    (if (duplicates? vars)
      (error 'desugar-let "OAZO: Duplicate binding in let: ~a" vars)
      (match bs
        ['() (appC (lamC (reverse vars) body) (reverse exprs))]
        [(cons (cons var expr) rest)
         (desugar rest (cons var vars) (cons expr exprs))]))))

; Large parse test
(check-equal? (parse
  '{let 
    {x <- 10}
    {y <- 5}
    {z <- true}
    {add <- {anon {x y} : {+ x y}}}
    {if z then {add x y} else 6}})
  (appC (lamC (list 'x 'y 'z 'add) (ifC (idC 'z) (appC (idC 'add) (list (idC 'x) (idC 'y))) (numC 6))) 
  (list (numC 10) (numC 5) (idC 'true) (lamC (list 'x 'y) (appC (idC '+) (list (idC 'x) (idC 'y)))))))

; Test parsing of let expressions:
(check-equal? (parse '{let {z <- 5} z}) (appC (lamC (list 'z) (idC 'z)) (list (numC 5))) )

; Test parsing of numbers
(check-equal? (parse 42) (numC 42))

; More handin tests
(check-exn (regexp "OAZO: Tried to use reserved symbol") (lambda () (parse '(+ if 4))))
(check-exn (regexp "OAZO: Duplicate args for function") (lambda () (parse '(anon (x x) : 3))))
(check-exn (regexp "OAZO: Duplicate binding in let") (lambda () (parse '(let (z <- (anon () : 3)) (z <- 9) (z) ))))

; Test parsing of strings
(check-equal? (parse "hello") (strC "hello"))

; Test parsing of if expressions
(check-equal? (parse '(if true then 1 else 2)) (ifC (idC 'true) (numC 1) (numC 2)))

; Test parsing of anonymous functions
(check-equal? (parse '(anon (x) : x)) (lamC (list 'x) (idC 'x)))
(check-equal? (parse '(anon () : 5)) (lamC '() (numC 5)))
(check-equal? (parse '(anon (x y) : (+ x y))) (lamC (list 'x 'y) (appC (idC '+) (list (idC 'x) (idC 'y)))))

; Test parsing of function applications
(check-equal? (parse '(f x)) (appC (idC 'f) (list (idC 'x))))

; Invalid args
(check-exn (regexp "OAZO: Invalid parameter") (lambda () (parse '(anon ("Invalid") : x))))
(check-exn (regexp "OAZO: Expected list of parameters, got") (lambda () (parse '(anon "Invalid" : x))))

; Test unsupported expressions
(check-exn (regexp "OAZO: Unsupported expression") (lambda () (parse (make-vector 3 2))))

; TESTS FROM handin
(check-exn (regexp "OAZO: No body for let") (lambda () (parse '{let {x <- 5}})))
(check-exn (regexp "OAZO: Reserved symbol used in if expression") (lambda () (parse '{if : then 0 else 1})))

; Test invalid let binding
(check-exn (regexp "OAZO: Invalid let binding") (lambda () (parse-let-binding '{"x" <- 5})))

; Test desugaring of let with one binding
(check-equal? (desugar-let (list (cons 'x (numC 5))) (idC 'x))
              (appC (lamC (list 'x) (idC 'x)) (list (numC 5))))
; Test desugaring of let with multiple bindings
(check-equal? (desugar-let (list (cons 'x (numC 5)) (cons 'y (numC 10))) (appC (idC '+) (list (idC 'x) (idC 'y))))
              (appC (lamC (list 'x 'y) (appC (idC '+) (list (idC 'x) (idC 'y)))) (list (numC 5) (numC 10))))


; The extend-env function takes a list of symbols and values 
; and an env and adds the bindings to the env
(define (extend-env [params : (Listof Symbol)] [arg-vals : (Listof Value)] [env : Env]) : Env
  (define new-bindings : (Listof Bind)
    (map (lambda ([param : Symbol] [arg-val : Value]) (Bind param arg-val)) params arg-vals))
  (Env (append new-bindings (Env-l env))))

; Test extend-env
(check-equal? (Env-l (extend-env (list 't 's) (list (NumV 5) (NumV 6)) 
              (Env (list (Bind 'x (NumV 10)) (Bind 'y (NumV 20))))))
              (list (Bind 't (NumV 5)) (Bind 's (NumV 6)) (Bind 'x (NumV 10)) (Bind 'y (NumV 20))))


; This function takes a symbol and a env and returns the value that maps to the sym
(define (lookup [s : Symbol] [e : Env]) : Value
  (match (Env-l e)
    ['() (error 'lookup "OAZO: Binding not found for: ~a" s)]
    [(cons (Bind n v) rest)
     (if (equal? s n)
         v ; Return numC so our interp can resolve this.
         (lookup s (Env rest)))]))

(check-equal? (lookup 'x (Env (list (Bind 'y (NumV 5)) (Bind 'x (NumV 8))))) (NumV 8))
(check-exn (regexp (regexp-quote "OAZO: Binding not found"))
           (lambda () (lookup 'x (Env (list (Bind 'y (NumV 5)) (Bind 'z (NumV 6)))))))


; Takes an expression and a list of function definitions and outputs a real number
; by evaluating the expressions and using subsitution within functions.
(define (interp [e : ExprC] [env : Env]) : Value
  (match e
    [(numC n) (NumV n)]
    [(strC str) (StrV str)]
    [(idC id) (lookup id env)]
    [(ifC test then else)
     (let ([test-result (interp test env)])
       (match test-result
         [(BoolV b) (if b (interp then env) (interp else env))]
         [_ (error 'interp "OAZO: IF test did not evaluate to a boolean")]))]
    [(lamC params body) (ClosV params body env)]
    [(appC fun args)
     (let ([fun-val (interp fun env)])
       (let ([arg-vals (map (lambda ([arg : ExprC]) (interp arg env)) args)])
         (match fun-val
           [(ErrorV error-func)
            (if (equal? (length arg-vals) 1)
                (exit (error-func (first arg-vals)))
                (error 'interp "OAZO: Error function called with more than one argument"))]
           [(ClosV params body closure-env)
            (if (= (length params) (length arg-vals))
                (interp body (extend-env params arg-vals closure-env))
                (error 'interp "OAZO: Incorrect number of arguments for closure"))]
           [(PrimVAny prim-func)
            (if (= (length arg-vals) 2)
                (prim-func (first arg-vals) (second arg-vals))
                (error 'interp "OAZO: Incorrect number of arguments for primitive function"))]
           [(PrimV prim-func)
            (let ([arg1 (first arg-vals)] [arg2 (second arg-vals)])
              (cond
                [(and (= (length arg-vals) 2) (NumV? arg1) (NumV? arg2)) (prim-func arg1 arg2)]
                [else
                 (error 'interp "OAZO: Incorrect number of arguments/type for primitive function")]))]     
           [_ (error 'interp "OAZO: Attempt to call a non-function value")])))]))

(check-equal? (interp (appC (lamC '() (numC 8)) '()) base-env) (NumV 8)) ; THIS WORKS

; Setup a test environment
(define test-env
  (Env (list (Bind 'x (NumV 5))
             (Bind 'y (BoolV #f))
             (Bind 'z (StrV "test")))))

; Test evaluation of numbers
(check-equal? (interp (numC 42) base-env) (NumV 42))

; Test evaluation of strings
(check-equal? (interp (strC "hello") base-env) (StrV "hello"))

; Test evaluation of if expressions
(check-equal? (interp (ifC (idC 'true) (numC 1) (numC 2)) base-env) (NumV 1))
(check-equal? (interp (ifC (idC 'false) (numC 1) (numC 2)) base-env) (NumV 2))

; Test evaluation of anonymous functions and applications
(check-equal? (interp (appC (lamC (list 'a) (numC 1)) (list (numC 2))) base-env) (NumV 1))

; Test evaluation of anonymous with primitives
(check-equal? (interp (appC (lamC (list 'a) (appC (idC '+) (list (numC 2) (idC 'a)))) (list (numC 2))) base-env) 
  (NumV 4))

; Test evaluation with environment
(check-equal? (interp (idC 'x) test-env) (NumV 5))

; Test error on undefined variable
(check-exn (regexp "OAZO: Binding not found") (lambda () (interp (idC 'undefined) base-env)))

; Test ErrorV
(check-exn (regexp "user-error: 5") (lambda () (interp (appC (idC 'error) 
  (list (numC 5))) base-env)))
(check-exn (regexp "OAZO: Error function called with more than one argument") (lambda () 
  (interp (appC (idC 'error) (list (numC 5) (numC 6))) base-env)))

; Test PrimVAny
(check-equal? (interp (appC (idC 'equal?) (list (numC 5) (numC 5))) base-env) (BoolV #t))
(check-exn (regexp "OAZO: Incorrect number of arguments for primitive function") (lambda () 
  (interp (appC (idC 'equal?) (list (numC 5))) base-env)))

; Test incorrect arguments
(check-exn (regexp "OAZO: Incorrect number of arguments for closure") (lambda () 
  (interp (appC (lamC (list 'a) (numC 1)) (list (numC 2) (numC 3))) base-env)))
(check-exn (regexp "OAZO: Incorrect number of arguments/type for primitive function") (lambda () 
  (interp (appC (idC '+) (list (numC 2) (numC 3) (numC 4))) base-env)))

; Test attempt to call a non-function value
(check-exn (regexp "OAZO: Attempt to call a non-function value") (lambda () 
  (interp (appC (numC 5) (list (numC 2))) base-env)))

; Test if not evaluating to a boolean
(check-exn (regexp "OAZO: IF test did not evaluate to a boolean") (lambda () 
  (interp (ifC (numC 5) (numC 1) (numC 2)) base-env)))


; top-interp takes a s-epression and combines the parser and interperater that we made
; to produce a real number
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) base-env)))

; Top interp tests
(check-equal? (top-interp
  '{let 
    {x <- 10}
    {y <- 5}
    {z <- true}
    {add <- {anon {x y} : {+ x y}}}
    {if z then {add x y} else 6}})
    "15")
