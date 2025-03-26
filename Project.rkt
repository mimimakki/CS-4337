#lang racket

; Detects the mode: interactive or batch
(define interactive?
  (let ((args (current-command-line-arguments)))
    (cond
      [(= (vector-length args) 0) #t]
      [(string=? (vector-ref args 0) "-b") #f]
      [(string=? (vector-ref args 0) "--batch") #f]
      [else #t])))

(define (process-tokens token history)
  (cond
    [(string->number token)] ; If the token is already a valid number, simply return it   
    [(and (> (string-length token) 1) (char=? (string-ref token 0) #\$)) ; Checks if token ($) is followed by an index
     (let ([n (string->number (substring token 1))]) ; Converts said index to a number
       (if (and (integer? n) (> n 0) (<= n (length history))) ; If index is valid and within bounds, retreive the value from history
           (list-ref (reverse history) (sub1 n))
           'error))] ; If invalid or out of bounds, return error
    [else token])) ; Simply return token if not a valid number or history id 

(define (tokenize input)
  (define (helper chars acc current) ; Accumulator
    (cond
      [(null? chars) ; Base case
       (reverse (if (string=? current "") acc (cons current acc)))]
      [(char-whitespace? (car chars)) ; Handles whitespace
       (helper (cdr chars) (if (string=? current "") acc (cons current acc)) "")]
      [(member (car chars) '(#\+ #\- #\* #\/)) ; Handles operators
       (helper (cdr chars) (cons (string (car chars)) (if (string=? current "") acc (cons current acc))) "")]
      [(char=? (car chars) #\$) ; Handles history ids
       (helper (cdr chars) acc "$")]
      [(and (string=? current "$") (char-numeric? (car chars)))
       (helper (cdr chars) acc (string-append current (string (car chars))))]
      [(char-numeric? (car chars)) ; Handles numbers
       (helper (cdr chars) acc (string-append current (string (car chars))))]
      [else ; Handles invalid cases and resets current
       (helper (cdr chars) (if (string=? current "") acc (cons current acc)) "")]))
  (helper (string->list input) '() ""))
