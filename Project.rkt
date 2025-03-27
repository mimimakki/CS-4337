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
       (helper (cdr chars) (if (string=? current "") acc (cons current acc)) "$")]
      [(and (string=? current "$") (char-numeric? (car chars)))
       (helper (cdr chars) acc (string-append current (string (car chars))))]
      [(char-numeric? (car chars)) ; Handles numbers
       (helper (cdr chars) acc (string-append current (string (car chars))))]
      [else 'error])) ; Returns error if token is invalid
  (helper (string->list input) '() ""))

  
(define (eval-operation operator rest history)
  (let-values ([(operand1 remaining1) (eval-expression rest history)])
    (if (or (eq? operand1 'error) (null? remaining1))  ; Returns error if first operand is invalid
        (values 'error '()) 
        (let-values ([(operand2 remaining2) (eval-expression remaining1 history)])
          (if (or (eq? operand2 'error)   ; Returns error if second operand is invalid
                  (and (eq? operator /) (= operand2 0)))  ; Returns error if division by zero
              (values 'error '())  
              (values (operator operand1 operand2) remaining2))))))


(define (eval-expression tokens history)
  (if (or (null? tokens) (eq? tokens 'error)) ; Base case, if tokens are null or contains error
      (values 'error '()) 
      (let* ([token (car tokens)] ; Processes first token using car
             [rest (cdr tokens)] ; Remaining tokens are processed using cdr 
             [parsed (process-tokens token history)]) 
        (cond
          [(eq? parsed 'error) (values 'error '())] ; Stop further processing if error
          [(number? parsed) (values parsed rest)] ; Valid number is parsed 
          [(string=? parsed "+") ; Addition operator (+) is parsed 
           (eval-operation + rest history)] 
          [(string=? parsed "*")  ; Multiplication operator (*) is parsed
           (eval-operation * rest history)] 
          [(string=? parsed "/")  ; Division operator (/) is parsed
           (eval-operation / rest history)]  
          [(string=? parsed "-")  ; Negation operator (-) is parsed
           (let-values ([(operand remaining) (eval-expression rest history)])
             (if (eq? operand 'error) (values 'error '()) 
                 (values (- operand) remaining)))]  ; Apply negation
          [else (values 'error '())])))) 


(define (eval-loop history) ; The history is a parameter to the eval loop function 
  (when interactive? (display "Enter expression: ")) ; The program immediately prompts the user for an expression
  (let ([input (read-line (current-input-port) 'any)]) ; Reads input 
    (cond
      [(eof-object? input) (void)] ; Loop exits by returning void if user enters eof in terminal 
      [(string=? input "quit")
       (when interactive? (displayln "Program Ended")) (void)] ; Program exits if user enters 'quit'
      [else
        (let* ([tokens (tokenize input)]) ; Converts the input string into list of tokens 
          (let-values ([(value remaining) (eval-expression tokens history)]) ; Parses and evaluates tokenized expression 
            (cond
              [(eq? value 'error)
               (displayln "Error: Invalid Expression")
               (eval-loop history)] ; Recursively calls eval-loop 
              [(null? remaining)
               (let* ([new-history (cons value history)] ; Adding a value to history involves cons-ing it with the existing history
                      [index (length new-history)])
                   (display index) (display ": ") ; The printed result is prefixed by the history id 
                 (displayln (real->double-flonum value)) ; Number is converted to a float and then printed with display
                 (eval-loop new-history))]
              [else
               (displayln "Error: Invalid Expression")
               (eval-loop history)])))])))

(define (main)
  (eval-loop '()))

(main)
