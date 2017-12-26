#lang racket

(define fileName (vector-ref (current-command-line-arguments) 0))
(define myline (file->lines fileName))
(define mylist (map string->number (string-split (car myline))))
(define rest (cdr mylist))
(define N (car mylist))
(define D (cadr mylist))
(define k (caddr mylist))
(define e (cadddr mylist))
(define MinPts (cadddr rest))
;Extracting the parameters from the first line of mylist
(define const 1)
(define (my-sum lst)                                                               ;Function to calculate the sum of the elements of a list
 (foldr + 0 lst))

(define comprise member)                                                           ;Function to check if a given list contains a value

(define (diff list1 list2) (if (null? list2)                                       ; Function to calculate the difference between two lists
      null
      (if (comprise (car list2) list1)
          (diff list1 (cdr list2) )
          (cons (car list2) (diff list1 (cdr list2))))))

(define (sum-squares lst)                                                          ;Function to calculate the sum of the squares of the elements of a list
  (apply + (map (lambda (x) (expt x 2)) lst)))
(define (sub-list lst1 lst2) (map (lambda (x y) (- x y)) lst1 lst2))               ;Function to calculate the difference between the corresponding elements of two lists 
(define pointlines (cdr myline))
(define (euc-dist list1 list2) (sqrt(sum-squares (sub-list list1 list2))))         ;Function to calculate the Euclidean distance between two data points

(define mypoints (build-list N (lambda (x)
 (map string->number (string-split (list-ref pointlines x))) )))

(define step1 (build-list N (lambda (x)                                            
  (list (+ x 1) (list-ref mypoints x)))))
;(display step1)
;(newline)
; Part 1 over