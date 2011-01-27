(use srfi-27)

(define (get-n pile n)
  (cond
   ((null? pile) -1)
   ((= n 0) (car pile))
   (else (get-n (cdr pile) (- n 1)))))

(define (delete-n pile n)
  (cond
   ((null? pile) '())
   ((= n 0) (cdr pile))
   (else (cons (car pile) (delete-n (cdr pile) (- n 1))))))

(define (swap-top pile n)
  (cond
   ((null? pile) '())
   (else (cons (get-n pile n) (delete-n pile n)))))

(define (shuffle pile)
  (define (re-shuffle pile)
    (cond
     ((null? pile) '())
     (else (cons (car pile) (shuffle (cdr pile))))))
  (cond
   ((null? pile) '())
   (else (re-shuffle (swap-top pile (random-integer (length pile)))))))

(define trunp-pile
  (fold
   (lambda (mark prevNums)
     (append
      (fold 
       (lambda (num prev)
	 (cons (cons mark num) prev))
       '()
       '(1 2 3 4 5 6 7 8 9 10 11 12 13))
      prevNums))
   '()
   '(:H :D :S :C)))

(shuffle trunp-pile)
