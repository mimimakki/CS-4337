#lang racket

; Detects the mode: interactive or batch
(define prompt?
  (let ((args (current-command-line-arguments)))
    (cond
      [(= (vector-length args) 0) #t]
      [(string=? (vector-ref args 0) "-b") #f]
      [(string=? (vector-ref args 0) "--batch") #f]
      [else #t])))


(define (parse-input input history)
  (cond
    [(string->number input) ; If input is already valid, simply return it 
     (string->number input)] 
    [(and (> (string-length input) 1) ; Check if it is referencing a certain index from history
          (char=? (string-ref input 0) #\$)) 
     (let* ([n (string->number (substring input 1))] 
            [rev-history (reverse history)]) 
       (if (and (integer? n) (> n 0) (<= n (length rev-history)))
           (list-ref rev-history (sub1 n)) 
           'error))]
    [else input])) 