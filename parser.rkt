#lang racket

;stack
(define stack (list "$$" "program"))

;what line we on
(define li 1)

;log err
(define (log-err)
  (displayln (~a "Err in line " li)))

;return first item in stack
(define (buffer_first)
  (last stack))

;remove from stack
(define (buffer_remove)
  (set! stack (drop-right stack 1)))

;push to stack, must be list
(define (buffer_add item)
  (set! stack (append stack item)))
  

;predict set of name with value
(define predict_name (make-hash))
(hash-set! predict_name "program" "stmt_list")
(hash-set! predict_name "stmt_list" "htyy")

;following are predict set ftns
(define (program item)
  (buffer_remove)
  (if (not (string=? "$$" item)) (buffer_add (list "stmt_list")) (displayln "Hvac working..."))) 
(define (stmt_list item)
  (buffer_remove)
  (if (not (string=? "$$" item)) (buffer_add (list "stmt_list" "stmt")) (displayln "WE DID IT!")))
(define (stmt item)
  (buffer_remove)
  (cond
    [(string=? "id" item) (buffer_add (list "expr" "=" "id"))]
    [(string=? "read" item) (buffer_add (list "id" "read"))]
    [(string=? "write" item) (buffer_add (list "expr" "write"))]))
(define (expr)
  (buffer_remove)
  (buffer_add (list "term_tail" "term")))
(define (term_tail item)
  (buffer_remove)
  (cond
    [(or (string=? "+" item) (string=? "-" item)) (buffer_add (list "term_tail" "term" "add_op"))]))
(define (term)
  (buffer_remove)
  (buffer_add (list "factor_tail" "factor")))
(define (factor_tail item)
  (buffer_remove)
  (cond
    [(or (string=? "*" item) (string=? "/" item)) (buffer_add (list "factor_tail" "factor" "mult_op"))]))
(define (factor item)
  (buffer_remove)
  (cond
    [(string=? "(" item) (buffer_add (list ")" "expr" "("))]
    [(string=? "id" item) (buffer_add (list "id"))]
    [(number? item) (buffer_add (list item))]))
(define (add_op item)
  (buffer_remove)
  (cond
    [(string=? "+" item) (buffer_add (list "+"))]
    [(string=? "-" item) (buffer_add (list "-"))]))
(define (mult_op item)
  (buffer_remove)
  (cond
    [(string=? "*" item) (buffer_add (list "*"))]
    [(string=? "/" item) (buffer_add (list "/"))]))

;big boy recursion, but not really
(define (predict item)
  (displayln item)
  ;make sure single char string are ids and not numbers or paranthesis or oper
  (if (and (=(string-length item) 1) (not(number? item)) (not(string=? item "(")) (not(string=? item "+")) (not(string=? item "-")) (not(string=? item "*")) (not(string=? item "/")) (not(string=? item ")"))) (set! item "id") (displayln ".."))

  (cond
    ;if stack at program and input correct
    [(string=? (buffer_first) "program")
     (if (or (string=? item "id") (string=? item "read") (string=? item "write") (string=? item "$$"))
         (program item)
         ((displayln "err in program")(log-err)(exit #t))) ]
    ;if stack at stmt_list and input correct
    [(string=? (buffer_first) "stmt_list")
     (if (or (string=? item "id") (string=? item "read") (string=? item "write") (string=? item "$$"))
         (stmt_list item)
         ((displayln "err in stmt_list")(log-err)(exit #t))) ]
    ;if stack at stmt and input correct
    [(string=? (buffer_first) "stmt")
     (if (or (string=? item "id") (string=? item "read") (string=? item "write"))
         (stmt item)
         ((displayln "err in stmt")(log-err)(exit #t))) ]
    ;if stack at expr and input correct
    [(string=? (buffer_first) "expr")
     (if (or (string=? item "(") (string=? item "id") (number? item))
         (expr)
         ((displayln "err in expr")(log-err)(exit #t))) ]
    ;if stack at term_tail and input correct
    [(string=? (buffer_first) "term_tail")
     (if (or (string=? item "+") (string=? item "-") (string=? item ")") (string=? item "id") (string=? item "read") (string=? item "write") (string=? item "$$"))
         (term_tail item)
         ((displayln "err in term_tail")(log-err)(exit #t))) ]
    ;if stack at term and input correct
    [(string=? (buffer_first) "term")
     (if (or (string=? item "(") (string=? item "id") (number? item))
         (term)
         ((displayln "err in term")(log-err)(exit #t))) ]
    ;if stack at factor_tail and input correct
    [(string=? (buffer_first) "factor_tail")
     (if (or (string=? item "*") (string=? item "/") (string=? item "+") (string=? item "-") (string=? item ")") (string=? item "id") (string=? item "read") (string=? item "write") (string=? item "$$"))
         (factor_tail item)
         ((displayln "err in factor_tail")(log-err)(exit #t))) ]
    ;if stack at factor and input correct
    [(string=? (buffer_first) "factor")
     (if (or (string=? item "(") (string=? item "id") (number? item))
         (factor item)
         ((displayln "err in factor")(log-err)(exit #t))) ]
    ;if stack at add_op and input correct
    [(string=? (buffer_first) "add_op")
     (if (or (string=? item "+") (string=? item "-"))
         (add_op item)
         ((displayln "err in add_op")(log-err)(exit #t))) ]
    ;if stack at mult_op and input correct
    [(string=? (buffer_first) "mult_op")
     (if (or (string=? item "*") (string=? item "/"))
         (mult_op item)
         ((displayln "err in mult_op")(log-err)(exit #t))) ]
    )
  (if (not (string=? item (buffer_first))) (predict item) (buffer_remove))
  (if (string=? item "$$") ((displayln "Accept")(exit #t)) (displayln "again"))
  (displayln "new stack, hop out recursion and do next thingy")
  (displayln stack))

;for lines where I have to split them apart and make them into strings
(define (process lst)
  (map (lambda (x)
        (string x)) lst)) 

;The man, the myth, the main program
(define (parse source)
  (for ([line (file->lines source)])
   (let ([x (string-split line)])
    (map (lambda (item)
       (if (or (string-contains? item "(") (string-contains? item ")")) (map predict (process(string->list item))) (predict item))) x)
    (displayln "end of list")
      (set! li (+ li 1)))))




