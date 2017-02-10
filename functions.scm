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
            ((and (not (list? x)) (not (number? x)) (cons x '())))
            
            ;first is to check the lhs if it is a list
            ((list? (lhs x))
                (cond
                    ;first we must check if we have a right hand side to evaluate
                    ; no rhs means we just take the nvar form the list lhs
                    ((null? (cdr (cdr x))) (cons (nvar (lhs x)) '()))
                    
                    ;check what is in the rhs
                    ((list? (rhs x)) (append (cons (nvar(lhs x)) '()) (nvar (rhs  x))) )
                    
                    ;as long as the thing in the rhs isn't a number we can add it to the list
                    ((not (number? (rhs x))) (append (cons (nvar(lhs x)) '())  (rhs  x)))
                    
                    ;we have entered a weird and wild case where the rhs should not be added to the list
                    (else (cons (nvar (lhs x)) '()))
                    
                )
            )
            
            ;check if our rhs exists
            ((null? (cdr (cdr x))) 
                ;inside here we have no rhs
                (cond
                    ;check if the lhs is a variable
                    ((not (number? (lhs x)) (cons (nvar (lhs x)) '())) )
                    
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
                    ((not (number? (lhs x))) (append (cons (lhs x) '())  (vars (rhs  x))))
                    
                    ;only add in the rhs variables in this case
                    (else (cons (vars (rhs x)) '()))
                )
            )
            
            ;in this case both the rhs and the lhs are not lists and the rhs exists
            ;check if the lhs is a number
            ((not (number? (lhs x)))
                (cond
                    ;this case the lhs and the rhs are both not numbers append them both 
                    ((not (number? (rhs x)))  (append     (cons  (lhs x) '()) (cons (rhs  x) '()))) 
                    ;otherwise only the lhs is not a number so just send up the lhs
                    (else (cons (lhs x) '()))
                )   
            )
            
            ;check if the rhs is not a number
            ((not (number? (rhs x)))  (cons (rhs x) '())  ) 
            
            ;we have no more variables and nore more list
             (else '())
        )
    )
)


