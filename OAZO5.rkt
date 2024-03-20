#lang typed/racket
(require typed/rackunit)

; Progress Toward Goal comment:
; - Full project implemented.

(define RESERVED_SYMBOLS : (Listof Symbol) '(if anon then else let : <-))
(define-type-alias Location Real)

; The OAZO5 Language:
(struct numC ([n : Real]) #:transparent)
(struct strC ([str : String]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct ifC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct appC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct lamC ([arg : (Listof Symbol)] [body : ExprC]) #:transparent) ; replaces our fundef

; Env stuff
(struct Bind ([name : Symbol] [val : Location]) #:transparent)
(struct Env ([l : (Listof Bind)]) #:transparent)

; Add values to the language
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StrV ([s : String]) #:transparent)
(struct ClosV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent) 
(struct PrimV ([func : (NumV NumV -> Value)]) #:transparent)
(struct PrimVArrEq ([func : (ArrayV ArrayV -> Value)]) #:transparent)
(struct PrimVArrRef ([func : (ArrayV NumV Store -> Value)]) #:transparent)
(struct PrimVArrSet ([func : (ArrayV NumV NumV Store -> Store)]) #:transparent)
(struct PrimVArrLen ([func : (ArrayV -> Value)]) #:transparent)
(struct PrimVBool ([func : (BoolV -> Value)]) #:transparent)
(struct PrimVStr ([func : (StrV StrV -> Value)]) #:transparent)

(struct ArrayV ([loc : Location] [len : Real]) #:transparent) ; new array value

(struct LibFuncVCombo ([func : ((Listof Value) -> Value)]) #:transparent)
(struct NullV () #:transparent)

(struct Cell ([loc : Location] [val : Value]) #:transparent) ; repr one cell in memory
(struct Store ([l : (Listof Cell)] [next-loc : Location]) #:transparent) ; Think of as memory
(struct V*S ([val : Value] [store : Store]) #:transparent) ; This is what interp returns now.
(struct B*NS ([loc : Location] [new-sto : Store]) #:transparent) ; Base location * new store, returned from allocate
(struct A*S ([vals : (Listof Value)] [new-sto : Store]) #:transparent) ; This is a list of evaluated args and the
; new store that results from evaluating the arguements.
(struct E*S ([env : Env] [sto : Store]) #:transparent) 

(define-type Value (U NumV BoolV StrV ClosV PrimV 
                      NullV LibFuncVCombo ArrayV PrimVArrEq PrimVArrRef PrimVArrSet
                      PrimVArrLen PrimVStr))
(define-type ExprC (U numC strC idC ifC lamC appC))


; This function takes a value and returns its string representation.
(define (serialize [v : Value]) : String
  (match v
    [(NumV n) (format "~v" n)]
    [(BoolV b) (if b "true" "false")]
    [(StrV s) (format "~v" s)]
    [(ClosV p b e) "#<procedure>"]
    [(PrimV f) "#<primop>"]
    [(NullV) "Null"]
    [(ArrayV loc len) (format "Array starting at ~v" loc)]))


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


; Primitive operation for num-eq?
(define (num-eq? [arg1 : NumV] [arg2 : NumV]) : Value
  (if (= (NumV-n arg1) (NumV-n arg2))
      (BoolV #t)
      (BoolV #f)))

(check-equal? (num-eq? (NumV 2) (NumV 3)) (BoolV #f))

; Primitive operation for str-eq?
(define (str-eq? [arg1 : StrV] [arg2 : StrV]) : Value
  (if (string=? (StrV-s arg1) (StrV-s arg2))
      (BoolV #t)
      (BoolV #f)))

(check-equal? (str-eq? (StrV "hi") (StrV "hi")) (BoolV #t))
(check-equal? (str-eq? (StrV "hi") (StrV "h")) (BoolV #f))

; Primitive operation to create an array
(define (arr [base : NumV] [size : NumV]) : Value
  (ArrayV (NumV-n base) (NumV-n size))) ; Touch this up

; Primitive operation to check of two arrays refer to the same array, returns true if they do false otherwise
(define (arr-eq? [a1 : ArrayV] [a2 : ArrayV]) : Value
  (if (= (ArrayV-loc a1) (ArrayV-loc a2))
      (BoolV #t)
      (BoolV #f)))
(check-equal? (arr-eq? (ArrayV 3 3) (ArrayV 3 3)) (BoolV #t))

; Given an array and an index, return the value in the array at the given index.
(define (aref [a : ArrayV] [ind : NumV] [sto : Store]) : Value
  (fetch (+ (ArrayV-loc a) (NumV-n ind)) sto))

; Given an array, index, and a new value, aset will set the index in the array to the new value.
(define (aset [a : ArrayV] [ind : NumV] [new : NumV] [sto : Store]) : Store
  (Store (set (+ (ArrayV-loc a) (NumV-n ind)) new (Store-l sto)) (Store-next-loc sto)))

; Takes a location, value, and list of cells and returns a new
; list of cells with the location changed to the value
(define (set [loc : Location] [val : Value] [cells : (Listof Cell)]) : (Listof Cell)
  (match cells
    ['() '()]
    [(cons f r)
     (if (= loc (Cell-loc f))
         (cons (Cell loc val) (set loc val r))
         (cons f (set loc val r)))]))

(check-equal? (set 1 (NumV 8) (list (Cell 0 (NumV 0)) (Cell 1 (NumV 0)) (Cell 2 (NumV 0))))
              (list (Cell 0 (NumV 0)) (Cell 1 (NumV 8)) (Cell 2 (NumV 0))))

; Take an array and return its length
(define (alen [a : ArrayV]) : Value
  (NumV (ArrayV-len a)))

(check-equal? (alen (ArrayV 0 4)) (NumV 4))

; Tests for our primitives
(check-equal? (prim+ (NumV 5) (NumV 5)) (NumV 10))
(check-equal? (prim- (NumV 10) (NumV 5)) (NumV 5))
(check-equal? (prim* (NumV 5) (NumV 5)) (NumV 25))
(check-equal? (prim/ (NumV 10) (NumV 5)) (NumV 2))
(check-equal? (prim<= (NumV 4) (NumV 5)) (BoolV #t))

; Library fuctions:

; Division by zero test
(check-exn (regexp "OAZO: Division by zero") (lambda () (prim/ (NumV 5) (NumV 0))))


; Base env
(define base-env
  (Env (list (Bind '+ 0)
        (Bind '- 1)
        (Bind '* 2)
        (Bind '/ 3)
        (Bind '<= 4)
        (Bind 'num-eq? 5)
        (Bind 'true 6)
        (Bind 'false 7)
        (Bind 'arr 8)
        (Bind 'arr-eq? 9)
        (Bind 'aref 10)
        (Bind 'aset 11)
        (Bind 'alen 12)
        (Bind 'str-eq? 13))))

; Base store
(define base-sto
  (Store (list  (Cell 0 (PrimV prim+))
        (Cell 1 (PrimV prim-))
        (Cell 2 (PrimV prim*))
        (Cell 3 (PrimV prim/))
        (Cell 4 (PrimV prim<=))
        (Cell 5 (PrimV num-eq?))
        (Cell 6 (BoolV #t))
        (Cell 7 (BoolV #f))
        (Cell 8 (PrimV arr))
        (Cell 9 (PrimVArrEq arr-eq?))
        (Cell 10 (PrimVArrRef aref))
        (Cell 11 (PrimVArrSet aset))
        (Cell 12 (PrimVArrLen alen))
        (Cell 13 (PrimVStr str-eq?)))
         14))

; Tests for serialize
(check-equal? (serialize (NumV 34)) "34")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (StrV "hello")) "\"hello\"")
(check-equal? (serialize (ClosV '() (numC 5) base-env)) "#<procedure>")
(check-equal? (serialize (PrimV prim+)) "#<primop>")
(check-equal? (serialize (ArrayV 7 8)) "Array starting at 7")
(check-equal? (serialize (NullV)) "Null")


; This function takes a store, number of locations to allocate, and a default value and
; returns the base location of what was just allocated along with the new store.

(define (allocate [sto : Store] [num-locs : Real] [dv : Value]) : B*NS
  (let ([base-loc (Store-next-loc sto)]) ; Starting location for allocation
    (let ([new-cells (for/list : (Listof Cell)([i : Real (in-range num-locs)])
                        (Cell (+ base-loc i) dv))]) ; Create new cells
      (let ([new-store (Store (append (Store-l sto) new-cells) ; Extend the store with new cells
                              (+ base-loc num-locs))]) ; Update next-loc
        (B*NS base-loc new-store))))) ; Return base location and new store

(check-equal? (allocate base-sto 4 (NumV 8)) (B*NS 14
                                                   (Store (list  (Cell 0 (PrimV prim+))
        (Cell 1 (PrimV prim-))
        (Cell 2 (PrimV prim*))
        (Cell 3 (PrimV prim/))
        (Cell 4 (PrimV prim<=))
        (Cell 5 (PrimV num-eq?))
        (Cell 6 (BoolV #t))
        (Cell 7 (BoolV #f))
        (Cell 8 (PrimV arr))
        (Cell 9 (PrimVArrEq arr-eq?))
        (Cell 10 (PrimVArrRef aref))
        (Cell 11 (PrimVArrSet aset))
        (Cell 12 (PrimVArrLen alen))
        (Cell 13 (PrimVStr str-eq?))
        (Cell 14 (NumV 8))
        (Cell 15 (NumV 8))
        (Cell 16 (NumV 8))
        (Cell 17 (NumV 8)))
                                                          18))) 



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
            (map (lambda ([param-pair : Sexp])
                   (match param-pair
                     [(list ty id) ; Match a list with two elements, a type and an identifier
                      (cond
                        [(and (symbol? ty) (symbol? id)) id] ; Check if both elements are symbols
                        [else (error 'parse "OAZO: Invalid parameter: ~a" param-pair)])]
                     [else (error 'parse "OAZO: Expected parameter pair, got: ~a" param-pair)]))
                 (if (list? params) 
                     params
                     (error 'parse "OAZO: Expected list of parameter pairs, got: ~a" params))))])
       (if (duplicates? checked-params)
           (error 'parse "OAZO: Duplicate args for function: ~a" checked-params)
           (lamC checked-params (parse body))))]
    [(list fun args ...) (appC (parse fun) (map parse args))]
    [_ (error 'parse "OAZO: Unsupported expression: ~a" s)]))

 
; This function parses the concrete syntax of a let binding into a list of symbol and expr
(define (parse-let-binding [b : Sexp]) : (Pair Symbol ExprC)
  (match b
    [(list (list id ': ty) '<- expr) 
      (cond 
        [(and (symbol? id) (not (member id RESERVED_SYMBOLS))) (cons id (parse expr))]
        [else (error 'parse-let-binding "OAZO: Invalid let binding: ~a" b)])]
    [_ (error 'parse-let-binding "OAZO: Invalid binding syntax: ~a" b)]))


(check-equal? (parse-let-binding '{[x : num] <- 5}) (cons 'x (numC 5)))
(check-exn (regexp "OAZO: Invalid binding syntax") (lambda () (parse-let-binding (cons 'x 5))))
(check-exn (regexp "OAZO: Invalid let binding")
           (lambda () (parse-let-binding '{[if : num] <- 5})))

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
    {[x : num] <- 10}
    {[y : num] <- 5}
    {[z : bool] <- true}
    {[add : num] <- {anon {[num x] [num y]} : {+ x y}}}
    {if z then {add x y} else 6}})
  (appC (lamC (list 'x 'y 'z 'add) (ifC (idC 'z) (appC (idC 'add) (list (idC 'x) (idC 'y))) (numC 6))) 
  (list (numC 10) (numC 5) (idC 'true) (lamC (list 'x 'y) (appC (idC '+) (list (idC 'x) (idC 'y)))))))

(check-exn (regexp "OAZO: Invalid parameter") (lambda () (parse
  '{let 
    {[x : num] <- 10}
    {[y : num] <- 5}
    {[z : bool] <- true}
    {[add : num] <- {anon {[num ()] [num y]} : {+ x y}}}
    {if z then {add x y} else 6}})))

; Test parsing of let expressions:
(check-equal? (parse '{let {[z : num] <- 5} z}) (appC (lamC (list 'z) (idC 'z)) (list (numC 5))) )

; Test parsing of numbers
(check-equal? (parse 42) (numC 42))

; More handin tests
(check-exn (regexp "OAZO: Tried to use reserved symbol") (lambda () (parse '(+ if 4))))
(check-exn (regexp "OAZO: Duplicate args for function") (lambda () (parse '(anon ([num x] [num x]) : 3))))
(check-exn (regexp "OAZO: Duplicate binding in let")
           (lambda () (parse '(let ([z : num] <- (anon () : 3)) ([z : num] <- 9) (z) ))))

; Test parsing of strings
(check-equal? (parse "hello") (strC "hello"))

; Test parsing of if expressions
(check-equal? (parse '(if true then 1 else 2)) (ifC (idC 'true) (numC 1) (numC 2)))

; Test parsing of anonymous functions
(check-equal? (parse '(anon ([num x]) : x)) (lamC (list 'x) (idC 'x)))
(check-equal? (parse '(anon () : 5)) (lamC '() (numC 5)))
(check-equal? (parse '(anon ([num x] [num y]) : (+ x y)))
              (lamC (list 'x 'y) (appC (idC '+) (list (idC 'x) (idC 'y)))))

; Test parsing of function applications
(check-equal? (parse '(f x)) (appC (idC 'f) (list (idC 'x))))

; Invalid args
(check-exn (regexp "parse: OAZO: Expected parameter pair, got: Invalid")
           (lambda () (parse '(anon ("Invalid") : x))))
(check-exn (regexp "parse: OAZO: Expected list of parameter pairs, got: Invalid")
           (lambda () (parse '(anon "Invalid" : x))))

; Test unsupported expressions
(check-exn (regexp "OAZO: Unsupported expression") (lambda () (parse (make-vector 3 2))))

; TESTS FROM handin
(check-exn (regexp "OAZO: No body for let") (lambda () (parse '{let {[x : num] <- 5}})))
(check-exn (regexp "OAZO: Reserved symbol used in if expression") (lambda () (parse '{if : then 0 else 1})))

; Test invalid let binding
(check-exn (regexp "OAZO: Invalid binding syntax:") (lambda () (parse-let-binding '{"x" <- 5}))) 

; Test desugaring of let with one binding
(check-equal? (desugar-let (list (cons 'x (numC 5))) (idC 'x))
              (appC (lamC (list 'x) (idC 'x)) (list (numC 5))))
; Test desugaring of let with multiple bindings
(check-equal? (desugar-let (list (cons 'x (numC 5)) (cons 'y (numC 10))) (appC (idC '+) (list (idC 'x) (idC 'y))))
              (appC (lamC (list 'x 'y) (appC (idC '+) (list (idC 'x) (idC 'y)))) (list (numC 5) (numC 10))))


; The extend-env function takes a symbol, location and an env and appends the new binding to the env.
(define (extend-env [sym : Symbol] [loc : Location] [env : Env]) : Env
  (Env (append (list (Bind sym loc)) (Env-l env))))

; Test extend-env
#;(check-equal? (Env-l (extend-env (list 't 's) (list (NumV 5) (NumV 6)) 
              (Env (list (Bind 'x (NumV 10)) (Bind 'y (NumV 20))))))
              (list (Bind 't (NumV 5)) (Bind 's (NumV 6)) (Bind 'x (NumV 10)) (Bind 'y (NumV 20))))

(check-equal? (Env-l (extend-env 'test 108 base-env))
              (append (list (Bind 'test 108)) (Env-l base-env)))

; This function takes a list of pararms, arguements, an environment and a store, and
; returns a new env and sto both extended with the new params and arguments. 
(define (extend-env&sto [params : (Listof Symbol)] [arg-vals : (Listof Value)] [env : Env] [sto : Store]) : E*S
  (match (list params arg-vals)
    [(list '() '()) (E*S env sto)]
    [(list (cons p p-rest) (cons a arg-rest))
     (let* ([loc-newSto (allocate sto 1 a)]
           [new-sto (B*NS-new-sto loc-newSto)]
           [loc (B*NS-loc loc-newSto)]
           [new-env (extend-env p loc env)])
       (extend-env&sto p-rest arg-rest new-env new-sto))]))


; This function takes a location and a store and fetechs the value at the location in the store.
(define (fetch [l : Location] [s : Store]) : Value
  (match (Store-l s)
    ['() (error 'fetch "OAZO: Location not in store for ~a" l)]
    [(cons (Cell loc v) rest)
     (if (equal? l loc)
         v
         (fetch l (Store rest (Store-next-loc s))))]))


(define test-sto1 (Store (list (Cell 1 (NumV 2)) (Cell 3 (NumV 8))) 4))

(check-equal? (fetch 3 test-sto1) (NumV 8))
(check-exn (regexp (regexp-quote "OAZO: Location not in store for"))
           (lambda () (fetch 2 (Store (list (Cell 1 (NumV 2)) (Cell 3 (NumV 8))) 4))))

; This function takes a symbol env and a store and returns the Value that maps to the sym
(define (lookup [s : Symbol] [e : Env] [sto : Store]) : Value
  (match (Env-l e)
    ['() (error 'lookup "OAZO: Binding not found for: ~a" s)]
    [(cons (Bind n l) rest)
     (if (equal? s n)
         (fetch l sto) ; Return numC so our interp can resolve this.
         (lookup s (Env rest) sto))]))



(check-equal? (lookup 'x (Env (list (Bind 'y 3) (Bind 'x 8)))
                      base-sto)  (PrimV arr))
(check-exn (regexp (regexp-quote "OAZO: Binding not found"))
           (lambda () (lookup 'x (Env (list (Bind 'y 7) (Bind 'z 6))) test-sto1))) 


;(check-equal? (eval-args-l2r (list (numC 2) (numC 3)) base-env base-sto) (list (NumV 2 ) (NumV 3)))

; Add a more complex test case with mutation between arguments later. 

; Takes an expression and a list of function definitions and outputs a real number
; by evaluating the expressions and using subsitution within functions.
(define (interp [e : ExprC] [env : Env] [sto : Store]) : V*S
  ; This function evalates the arguements of a function call from left to right
  ; it takes as input a list of expressions and interperates them all, returning a list of
  ; values, each coresponding to the orginal expression.
  (define (eval-args-l2r [args : (Listof ExprC)] [env : Env] [sto : Store]) : A*S
    (match args
      ['() (A*S args sto)]
      [(cons f r)
       (let* ([val-sto (interp f env sto)]
              [next-A*S (eval-args-l2r r env (V*S-store val-sto))])
         (A*S (cons (V*S-val val-sto) (A*S-vals next-A*S)) (A*S-new-sto next-A*S)))]))
  (match e
    [(numC n) (V*S (NumV n) sto)]
    [(strC str) (V*S (StrV str) sto)]
    [(idC id) (V*S (lookup id env sto) sto)]
    [(ifC test then else)
     (match (interp test env sto)
       [(V*S (BoolV #t) sto2) (interp then env sto2)]
       [(V*S (BoolV #f) sto2) (interp else env sto2)]
       [_ (error 'interp "OAZO: IF test did not evaluate to a boolean")])]
    [(lamC params body) (V*S (ClosV params body env) sto)]
    [(appC fun args)
     (let* ([new-V*S1 (interp fun env sto)]
            [args-store (eval-args-l2r args env (V*S-store new-V*S1))]
            [fun-val (V*S-val new-V*S1)]
            [arg-vals (A*S-vals args-store)]
            [new-store (A*S-new-sto args-store)])
       (match fun-val
           [(ClosV params body closure-env) 
            (if (= (length params) (length arg-vals))
                (let* ([new-env-sto (extend-env&sto params arg-vals env sto)]
                       [new-sto (E*S-sto new-env-sto)]
                       [new-env (E*S-env new-env-sto)])
                  (interp body new-env new-sto)) ; Figure out how to extend the environment
                (error 'interp "OAZO: Incorrect number of arguments for closure"))]
           [(PrimVStr prim-func)
            (match arg-vals
              [(list (? StrV? s1) (? StrV? s2)) (V*S (prim-func s1 s2) new-store)] 
              [else (error 'interp "OAZO: Incorrect number of arguments/type for primitive function str-eq?")])]
           [(PrimV prim-func)
            (if (eq? prim-func arr)
                (match arg-vals
                  [(list (? NumV? n1) (? NumV? n2))
                   (let ([base-ns (allocate new-store (NumV-n n1) n2)])
                     (V*S (prim-func (NumV (B*NS-loc base-ns)) (first arg-vals)) (B*NS-new-sto base-ns)))]
                  [else (error 'interp "OAZO: Incorrect number of arguments/type for primitive function arr")])
             (let ([arg1 (first arg-vals)] [arg2 (second arg-vals)])
              (cond
                [(and (= (length arg-vals) 2) (NumV? arg1) (NumV? arg2)) (V*S (prim-func arg1 arg2) new-store)]
                [else
                 (error 'interp "OAZO: Incorrect number of arguments/type for primitive function")])))]
           [(PrimVArrEq prim-func)
            (match arg-vals
              [(list (? ArrayV? a1) (? ArrayV? a2)) (V*S (prim-func a1 a2) new-store)]
              [else (error 'interp "OAZO: Incorrect number of arguments/type for primitive function arr-eq?")])]
           [(PrimVArrRef prim-func)
            (match arg-vals
              [(list (? ArrayV? a) (? NumV? ind)) (V*S (prim-func a ind new-store) new-store)]
              [else (error 'interp "OAZO: Incorrect number of arguments/type for primitive function aref")])]
           [(PrimVArrSet prim-func)
            (match arg-vals
              [(list (? ArrayV? a) (? NumV? idx) (? NumV? new))
               (let ([n-sto (prim-func a idx new new-store)])
                 (V*S (NullV) n-sto))]
              [else (error 'interp "OAZO: Incorrect number of arguments/type for primitive function aset")])]
           [(PrimVArrLen prim-func)
            (match arg-vals
              [(list (? ArrayV? a)) (V*S (prim-func a) new-store)]
              [else (error 'interp "OAZO: Incorrect number of arguments/type for primitive function alen")])]
           ;[(LibFuncVCombo lib-func) ; our seq implementation
            ;   (if (> (length arg-vals) 0)
             ;    (V*S (lib-func arg-vals) new-store)
              ;   (error 'interp "OAZO: Incorrect number of arguments for library function"))]
           [_ (error 'interp "OAZO: Attempt to call a non-function value")]))]))

; check errors

(check-exn (regexp "OAZO: Incorrect number of arguments/type for primitive function str-eq?")
           (lambda () (interp (appC (idC 'str-eq?) (list (strC "hi"))) base-env base-sto)))

(check-exn (regexp "OAZO: Incorrect number of arguments/type for primitive function arr")
           (lambda () (interp (appC (idC 'arr) (list (numC 8))) base-env base-sto)))

(check-exn (regexp "OAZO: Incorrect number of arguments/type for primitive function arr-eq?")
           (lambda () (interp (appC (idC 'arr-eq?) (list 
                            (appC (idC 'arr) (list (numC 2) (numC 10))))) base-env base-sto)))

(check-exn (regexp "OAZO: Incorrect number of arguments/type for primitive function aref")
           (lambda () (interp (appC (idC 'aref) (list (appC (idC 'arr) (list (numC 2) (numC 8))) ))
                      base-env base-sto)))

(check-exn (regexp "OAZO: Incorrect number of arguments/type for primitive function aset")
           (lambda () (interp
               (appC (idC 'aset) (list (appC (idC 'arr) (list (numC 2) (numC 8))) (numC 1) ))
                      base-env base-sto)))

(check-exn (regexp "OAZO: Incorrect number of arguments/type for primitive function alen")
           (lambda () (interp (appC (idC 'alen) (list)) base-env base-sto)))


; test string equal
(check-equal? (interp (appC (idC 'str-eq?) (list (strC "hi") (strC "hi"))) base-env base-sto)
              (V*S (BoolV #t) base-sto))

; test array set
(check-equal? (interp
               (appC (idC 'aset) (list (appC (idC 'arr) (list (numC 2) (numC 8))) (numC 1) (numC 18)))
                      base-env base-sto)
              (V*S (NullV)
                   (Store (append (Store-l base-sto)
                                  (list (Cell 14 (NumV 8)) (Cell 15 (NumV 18)))) 16)))
; test array len
(check-equal? (interp (appC (idC 'alen) (list (appC (idC 'arr) (list (numC 2) (numC 8))))) base-env base-sto)
              (V*S (NumV 2)
                   (Store (append (Store-l base-sto)
                                  (list (Cell 14 (NumV 8)) (Cell 15 (NumV 8)))) 16)))

; Test creating an array 
(check-equal? (interp (appC (idC 'arr) (list (numC 2) (numC 8))) base-env base-sto)
              (V*S (ArrayV 14 2)
                   (Store (append (Store-l base-sto)
                                  (list (Cell 14 (NumV 8)) (Cell 15 (NumV 8)))) 16)))


; Need to test arr-eq? with nested array creations- this one should be false
(check-equal? (interp (appC (idC 'arr-eq?) (list (appC (idC 'arr) (list (numC 2) (numC 8)))
                            (appC (idC 'arr) (list (numC 2) (numC 10))))) base-env base-sto)
              (V*S (BoolV #f)
                   (Store (append (Store-l base-sto)
                                  (list (Cell 14 (NumV 8)) (Cell 15 (NumV 8))
                                        (Cell 16 (NumV 10)) (Cell 17 (NumV 10)))) 18)))

; Test get a value from an array

; Use these definitions:
; define a test sto with one array
(define test-sto
  (Store (append (Store-l base-sto)
                                  (list (Cell 14 (NumV 8))
                                        (Cell 15 (NumV 8)))) 16))
(define test-arr
  (ArrayV 12 3))
(check-equal? (interp (appC (idC 'aref) (list (appC (idC 'arr) (list (numC 2) (numC 8))) (numC 1)))
                      base-env base-sto)
              (V*S (NumV 8) test-sto))


(check-equal? (interp (appC (lamC '() (numC 8)) '()) base-env base-sto) (V*S (NumV 8)
                                                                             base-sto)) 

; Test that binops still work
(check-equal? (V*S-val (interp (appC (idC '+) (list (numC 4) (numC 4))) base-env base-sto)) (NumV 8))


; Setup a test environment
(define test-env
  (Env (list (Bind 'x 5)
             (Bind 'y 4)
             (Bind 'z 6))))

; Test evaluation of numbers
(check-equal? (interp (numC 42) base-env base-sto) (V*S (NumV 42) base-sto))

; Test evaluation of strings
(check-equal? (interp (strC "hello") base-env base-sto) (V*S (StrV "hello") base-sto))

; Test evaluation of if expressions 
(check-equal? (interp (ifC (idC 'true) (numC 1) (numC 2)) base-env base-sto) (V*S (NumV 1) base-sto))
(check-equal? (interp (ifC (idC 'false) (numC 1) (numC 2)) base-env base-sto) (V*S (NumV 2) base-sto))

; Test evaluation of anonymous functions and applications
(check-equal? (interp (appC (lamC (list 'a) (numC 1)) (list (numC 2))) base-env base-sto)
              (V*S (NumV 1) (Store (append (Store-l base-sto)
                                  (list (Cell 14 (NumV 2)))) 15)))

; Test evaluation of anonymous with primitives
#;(check-equal? (interp (appC (lamC (list 'a)
  (appC (idC '+) (list (numC 2) (idC 'a)))) (list (numC 2))) base-env base-sto) (NumV 4))

; Test evaluation with environment
(check-equal? (interp (idC 'x) test-env base-sto) (V*S (PrimV num-eq?) base-sto)) 

; Test error on undefined variable
(check-exn (regexp "OAZO: Binding not found") (lambda () (interp (idC 'undefined) base-env base-sto)))


; Test PrimVAny
(check-equal? (interp (appC (idC 'num-eq?) (list (numC 5) (numC 5))) base-env base-sto) (V*S (BoolV #t) base-sto))
#;(check-exn (regexp "OAZO: Incorrect number of arguments for primitive function") (lambda () 
  (interp (appC (idC 'num-eq?) (list (numC 5))) base-env base-sto)))


; Test incorrect arguments
(check-exn (regexp "OAZO: Incorrect number of arguments for closure") (lambda () 
  (interp (appC (lamC (list 'a) (numC 1)) (list (numC 2) (numC 3))) base-env base-sto)))
(check-exn (regexp "OAZO: Incorrect number of arguments/type for primitive function") (lambda () 
  (interp (appC (idC '+) (list (numC 2) (numC 3) (numC 4))) base-env base-sto)))

; Test attempt to call a non-function value
(check-exn (regexp "OAZO: Attempt to call a non-function value") (lambda () 
  (interp (appC (numC 5) (list (numC 2))) base-env base-sto)))

; Test if not evaluating to a boolean
(check-exn (regexp "OAZO: IF test did not evaluate to a boolean") (lambda () 
  (interp (ifC (numC 5) (numC 1) (numC 2)) base-env base-sto)))


; top-interp takes a s-epression and combines the parser and interperater that we made
; to produce a real number
(define (top-interp [s : Sexp]) : String
  (serialize (V*S-val (interp (parse s) base-env base-sto))))

; Top interp tests
(check-equal? (top-interp
  '{let 
    {[x : num] <- 10}
    {[y : num] <- 5}
    {[z : num] <- true}
    {[add : num] <- {anon {[num x] [num y]} : {+ x y}}}
    {if z then {add x y} else 6}})
    "15")


