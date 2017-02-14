; Adam Johnson
; Assignment - 4
; 2/13/2017 Monday


; op - takes a function list (+ a b) and returns to operator
; Params - x a list with a function being applied to a left and right
; Returns - the operator from a list (+ a b) => +
(define op
    (lambda (x)
        (car x)
    )
)

; lhs - takes a function list (+ a b) and returns to left item
; Params - x a list with a function being applied to a left and right
; Returns - the left item (+ a b) => a
(define lhs
    (lambda (x)
        (car (cdr x))
    )
)

; lhs - takes a function list (+ a b) and returns to right item
; Params - x a list with a function being applied to a left and right
; Returns - the right item (+ a b) => b
(define rhs
    (lambda (x)
        (car (cdr (cdr x)))
        
    )
)

;member code from the book
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))
                
; make set takes a list and removes all duplicate entries
; params - x a list
; results - a list with all duplicates removed (a a b b c c) => ( a b c )
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else
        (cons (car lat) (makeset (cdr lat)))))))


(define vars
    (lambda (x)
        
        (makeset (nvar x))
    )
)

; nvar - extracts the variables from the list x skipping over the operators
; params - a list in the for (<operator> <a> <b>) where a and/or b can be either
;          a list, or an atom. If a or b are lists they can be null, but they can be formated in the same way.
;          The list can also be formated as (<operator> <a>).
; returns - a list of variables from the list passed in
(define nvar
    (lambda (x)
        (cond
            ;check if the list is null
            ((null? x) ())
            ;if we are handed an x and it is not a number just return the list containing x
            ((and (and (not (list? x)) (not (number? x))) (not (constant? x)))  (cons x '()) )
            
            ;first is to check the lhs if it is a list
            ((list? (lhs x))
                (cond
                    ;first we must check if we have a right hand side to evaluate
                    ; no rhs means we just take the nvar form the list lhs
                    ((null? (cdr (cdr x)))  (nvar (lhs x)) )
                    
                    ;check what is in the rhs
                    ((list? (rhs x)) (append (nvar(lhs x)) (nvar (rhs  x))) )
                    
                    ;as long as the thing in the rhs isn't a number we can add it to the list
                    ((not (number? (rhs x))) (append  (nvar(lhs x)))  (rhs  x))
                    
                    ;we have entered a weird and wild case where the rhs should not be added to the list
                    (else (cons (nvar (lhs x)) '()))
                    
                )
            )
            
            ;check if our rhs exists
            ((null? (cdr (cdr x))) 
                ;inside here we have no rhs
                (cond
                    ;check if the lhs is a variable
                    ((and (not (number? (lhs x))) (not (constant? (lhs x)))) (nvar (lhs x)) )
                    
                    ;do nothing if it is a number
                    (else '())
                )
            )
            
            
            ; at this point the lhs might just be a variable and the rhs might be a list
            ((list? (rhs x))
                (cond
                    ;inside here I know the rhs is a list and we have to do the same sanity checks we did for
                    ;the lhs above
                    
                    ;we can assume the lhs IS NOT NULL this is because we have a rhs thing so the lhs must be a thing
                    
                    ;check what is in the lhs if it isn't a number then add it into the list
                    ((and (not (constant? (lhs x) ) ) (not (number? (lhs x)))) (append (cons (lhs x) '())  (nvar (rhs  x))))
                    
                    ;only add in the rhs variables in this case
                    (else (cons (nvar (rhs x)) '()))
                )
            )
            
            ;in this case both the rhs and the lhs are not lists and the rhs exists
            ;check if the lhs is a number
            ((and (not (number? (lhs x))) (not (constant? (lhs x))))
                (cond
                    ;this case the lhs and the rhs are both not numbers append them both 
                    ((and (not (constant? (rhs x)))(not (number? (rhs x))))   (cons  (lhs x) (cons (rhs  x) '())))
                    ;otherwise only the lhs is not a number so just send up the lhs
                    (else (cons (lhs x) '()))
                )   
            )
                            
            ;check if the rhs is not a number
            ((and (not (constant? (rhs x)))(not (number? (rhs x))))   (rhs x) )   
            
            ;we have no more variables and nore more list
             (else '())
        )
    )
)


(define solve
    (lambda (x y)
       
        (let ((l (lhs x))
              (r (rhs x)))
            (cond
                ;((not (member? y (vars x))) '())
                ;if the lhs of x is y just return x
                ((equal? y l) x)
                ;if the rhs is y then return the list as (<op> <rhs> <lhs>)
                ((equal? y r)
                    `(= ,r ,l)
                )
            
                ;if the rhs is not equal to y and the rhs is not a list
                ((not (list? r)) '())
                
                ((unary r)
                    (let (
                        (rop (op r))
                        (lr (lhs r))
                         )
                        (cond
                        
                            ;lets do our sqrt case
                            ; (= a ( sqrt b))
                            ; (= (expt a 2) b)
                            ((equal? rop 'sqrt)
                                (solve `(= (expt ,l 2) ,lr)  y)
                            )
                            
                            ;lets do our exp case
                            ; (= a ( exp b))
                            ; (= (log a) b)
                            ((equal? rop 'exp)
                                (solve `(= (log ,l ) ,lr)  y)
                            )
                            
                            ;lets do our log case
                            ; (= a ( log b))
                            ; (= (exp a) b)
                            ((equal? rop 'log)
                                (solve `(= (exp ,l ) ,lr)  y)
                            )
                            
                            ;lets do our - unary case
                            ; (= a (- b))
                            ; (= (- a) b)
                            ((equal? rop '-)
                                (solve `(= (- ,l ) ,lr)  y)) 
                        )
                    )
                    
                )
            
                
                (else
                    (let (
                        (rop (op r))
                        (lr (lhs r))
                        (rr (rhs r))
                        )
                        
                        (cond
                            ;lets doo the addition case
                            ((equal? rop '+)
                                (let (
                                    (resl (solve `(= (- ,l ,lr) ,rr )  y) )
                                    )
                                    (cond
                                        ((null? resl) 
                                            (solve `(= (- ,l ,rr) ,lr ) y )
                                        )
                                        
                                        (else resl)
                                    )
                                
                                )
                            )
                            
                           ;lets doo the subtraction case!
                           ; (= a ( - b c)) 
                           ; (= (- b a) c) or (= (+ a c) b)
                           ((equal? rop '-) 
                                (let (
                                    (resl (solve `(= (- ,lr ,l) ,rr )  y) )
                                    )
                                    (cond
                                        ((null? resl) 
                                            (solve `(= (+ ,l ,rr) ,lr ) y )
                                        )
                                        
                                        (else resl)
                                    )
                                
                                )
                            )
                            
                            ;multiplication case
                            ; (= a (* b c)) 
                            ; (= (/ a b) c) or (= (/ a c) b)
                            ((equal? rop '*) 
                                (let (
                                    (resl (solve `(= (/ ,l ,lr) ,rr )  y) )
                                    )
                                    (cond
                                        ((null? resl) 
                                            (solve `(= (/ ,l ,rr) ,lr ) y )
                                        )
                                        
                                        (else resl)
                                    )
                                
                                )
                            )
                            
                            ;division case
                            ; (= a ( / b c)) 
                            ; (= (* a c) b) or (= (/ b a) (c))
                            ((equal? rop '/) 
                                (let (
                                    (resl (solve `(= (/ ,lr ,l) ,rr )  y) )
                                    )
                                    (cond
                                        ((null? resl) 
                                            (solve `(= (* ,l ,rr) ,lr ) y )
                                        )
                                        
                                        (else resl)
                                    )
                                )
                            )
                            
                            ;expt case
                            ; ( = a ( expt b 2))
                            ; (= (sqrt a) b)
                            ((equal? rop 'expt) 
                                (solve `(= (sqrt ,l) ,lr)  y)
                            )
                        )
                    )
                )
            )
        )
    )
)

; unary takes an expression and returns if the expression is unary
; params x - an expression in the form (<op> <l> <r?>) where <r?> could be optional
(define unary
    (lambda (x)
        (cond
            ((null? (cdr (cdr x))) #t)
            (else #f)
        )
    )
)

; Provided in the assignment rubric
; constant? returns whether or not an expression passed into it is a constant in the system
; param exp - something you want to know if it is a constant
; returns true if it is a constant or false if not
(define (constant? exp)
  (if (pair? exp) (eq? (car exp) 'quote) (not (symbol? exp))))


;(solve '(= a (+ b c)) 'b)
;(solve '(= a (* b c)) 'b)
;(solve '(= a (/ b c)) 'b)
;(solve '(= a (- b c)) 'b)
;(solve '(= a (- b)) 'b)
;(solve '(= a (sqrt b)) 'b)
;(solve '(= a (expt b 2)) 'b)
;