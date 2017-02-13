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
        (cond
            ;((not (member? y (vars x))) '())
            ;if the lhs of x is y just return x
            ((equal? y (lhs x)) x)
            ;if the rhs is y then return the list as (<op> <rhs> <lhs>)
            ((equal? y (rhs x))
                (cons (op x) (cons (rhs x) (cons (lhs x) '())))
            )
            
            ;if the rhs is not equal to y and the rhs is not a list
            ((not (list? (rhs x)))
                '()
            )
            (else
                ;at this point the rhs contains something we can solve for!
                (append (solve (moveleft x) y) (solve (moveright x) y) )
                ;(moveleft x)

            )
        )
    )
)

(define moveleft
    (lambda (x)
        (cond
            ;unary check
            ((unary (rhs x)) 
                (cond
                    ((equal? (op (rhs x)) '-)
                        ;so we just have to multiply both sides by negative 1
                        (cons
                            '=
                            (cons
                                (cons 
                                    '-
                                    (cons 
                                        (lhs x) '()
                                    )
                                
                                )
                                (cons
                                    (lhs (rhs x))
                                    '()
                                )
                            )
                        )
                     
                     
                     
                    )
                    
                    ((equal? (op (rhs x)) 'sqrt)
                        (cons
                            '=
                            (cons
                                (cons 
                                    'expt
                                    (cons 
                                        (lhs x) 
                                        (cons '2 '())
                                    )
                                
                                )
                                (cons
                                    (lhs (rhs x))
                                    '()
                                )
                            )
                        )
                        
                    )
                )
                
            )
            
            ;special case if the other side is expt n 2
            ((equal? (op (rhs x)) 'expt)
                ;the lhs becomes sqrt and the other side is just the lhs
                    (cons
                            '=
                            (cons
                                (cons 
                                    'sqrt
                                    (cons
                                    (lhs x) '())
                                
                                )
                                (cons
                                    (lhs (rhs x))
                                    '()
                                )
                            )
                        )
            )
            
            ;move left has some special rules with -, /, log
            ((equal? (op (rhs x)) '-)
                ;the lhs becomes sqrt and the other side is just the lhs
                (cons '= 
                    
                    (cons
                        ;to construct the left hand side
                        (cons
                            ;flip the op
                            (op (rhs x))
                            
                            ;then combine with 
                            (cons
                                ;the lhs of x
                                (lhs x) 
                                
                                ;and the lhs of the rhs of x
                                (cons (lhs (rhs x)) '())
                            )
                        )
                        
                        
                        (cons
                            ;this is just the rhs of the rhs of x
                            (cons 
                                '- 
                                (cons
                                    (rhs (rhs x))
                                    '()
                                )
                            )
                            '()
                        )

                    )
                    
                    
                )
            )
            
            ((equal? (op (rhs x)) '/)
                ;the lhs becomes sqrt and the other side is just the lhs
                (cons '= 
                    
                    (cons
                        (cons
                            (op (rhs x))
                            
                            (cons 
                                '1
                                ;to construct the left hand side
                                (cons

                                        (cons
                                            '*
                                            (cons
                                                ;and the lhs of the rhs of x
                                                (lhs (rhs x)) 
                                                
                                                ;the lhs of x
                                                (cons
                                                    (lhs x)
                                                    '()
                                                )
                                            )
                                        )
                                        '()

                                    
                                )
                            )
                        )
                        
                        (cons
                            ;this is just the rhs of the rhs of x

                            (rhs (rhs x))

                            
                            '()
                        )

                    )
                    
                    
                )
            )
            
            (else
                ;combine the equal sign with the proper lefthand side and righthand size
                (cons '= 
                    
                    (cons
                        ;to construct the left hand side
                        (cons
                            ;flip the op
                            (opflip (op (rhs x)))
                            
                            ;then combine with 
                            (cons
                                ;the lhs of x
                                (lhs x) 
                                
                                ;and the lhs of the rhs of x
                                (cons (lhs (rhs x)) '())
                            )
                        )
                        
                        

                        ;this is just the rhs of the rhs of x
                        (cons (rhs (rhs x)) '())

                    )
                    
                    
                )
            )
            
        )
    )
    
)

(define unary
    (lambda (x)
        (cond
            ((null? (cdr (cdr x))) #t)
            (else #f)
        )
    )
)

(define opflip
    (lambda (x)
        (cond
            ((equal? x '+) '-)
            ((equal? x '-) '+)
            ((equal? x '*) '/)
            ((equal? x '/) '*)
            ((equal? x 'exp) 'log)
            ((equal? x 'log) 'exp)
            ((equal? x 'expt) 'sqrt)
            ((equal? x 'sqrt) 'expt) 
        )
    )
)

;moveright takes the right hand term of the right h

(define moveright
    (lambda (x)
        (cond
            ;unary check
            ((unary (rhs x)) 
                (cond
                    ((equal? (op (rhs x)) '-)
                        ;so we just have to multiply both sides by negative 1
                        (cons
                            '=
                            (cons
                                (cons 
                                    '-
                                    (cons 
                                        (lhs x) '()
                                    )
                                
                                )
                                (cons
                                    (lhs (rhs x))
                                    '()
                                )
                            )
                        )

                    )
                    
                    ((equal? (op (rhs x)) 'sqrt)
                        (cons
                            '=
                            (cons
                                (cons 
                                    'expt
                                    (cons 
                                        (lhs x) 
                                        (cons '2 '())
                                    )
                                
                                )
                                (cons
                                    (lhs (rhs x))
                                    '()
                                )
                            )
                        )
                        
                    )
                )
                
            )
            
            ((equal? (op (rhs x)) 'expt)
                ;the lhs becomes sqrt and the other side is just the lhs
                    (cons
                            '=
                            (cons
                                (cons 
                                    'sqrt
                                    (cons
                                    (lhs x) '())
                                
                                )
                                (cons
                                    (lhs (rhs x))
                                    '()
                                )
                            )
                        )
            ) 
            
            
            
            (else 
                ;construct the new list
                (cons '= 
                    ;construct the left and right side
                    (cons
                        ;to construct the left hand side
                        (cons
                            ;flip the op
                            (opflip (op (rhs x)))
                            
                            ;then combine with 
                            (cons
                                ;the lhs of x
                                (lhs x) 
                                
                                ;and the lhs of the rhs of x
                                (cons (rhs (rhs x)) '())
                            )
                        )
                        
                        

                        ;this is just the rhs of the rhs of x
                        (cons (lhs (rhs x)) '())

                    )
                    
                    
                )
                
            )
        )
        
    )
)

(define (constant? exp)
  (if (pair? exp) (eq? (car exp) 'quote) (not (symbol? exp))))


;(solve '(= a (+ b c)) 'b)
;(solve '(= a (- b)) 'b)
;(solve '(= a (sqrt b)) 'b)
;(solve '(= a (expt b 2)) 'b)
;