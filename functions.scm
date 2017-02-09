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
