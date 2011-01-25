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
   (re-shuffle (swap-top pile (random-integer (length pile))))))