; 5.1 function arg-max
(define (findmax f max lis)
  (cond
    ((null? lis) max)
    (else (let ((sec (f (car lis))) (fir (f max)))
    (cond
      ((> fir sec) (findmax f max (cdr lis)))
      (else (findmax f (car lis) (cdr lis))))))))

(define (arg-max f lis)
  (findmax f (car lis) (cdr lis)))

(define (square n) (* n n))
(define (invert a) (/ 1000 a))

; 5.2 function zip
(define (zip . args)
  (car (cons args '())))

; 5.3 function unzip
(define (unzip lis num)
  (cond
    ((null? lis) '())
    (else (let((temp (- num 1)))
            (cond
              ((= num 0) (car lis))
              (else (unzip (cdr lis) temp)))))))

; 5.4 function intersectlist

(define (check num lis sym)
  (cond
    ((null? lis) (car sym))
    ((= num (car lis)) num)
    (else (check num (cdr lis) sym))))

(define (pen_intersectlist lis1 lis2 retlis)
  (cond
    ((null? lis1) retlis)
    (else (let* ((num (car lis1)) (numcheck (check num lis2 '(*))))
            (cond
              ((number? numcheck) (cons numcheck (pen_intersectlist (cdr lis1) lis2 retlis)))
              (else (pen_intersectlist (cdr lis1) lis2 retlis)))))))

(define (intersectlist lis1 lis2) (pen_intersectlist lis1 lis2 '()))

; 5.5 function sortedmerge

(define (merge retlis lis)
  (cond
    ((null? lis) retlis)
  (else (cons (car lis) (merge retlis (cdr lis))))))

(define (pre_sortedmerge lis1 lis2 retlis)
    (cond
      ((null? lis1) (merge retlis lis2))
      ((null? lis2) (merge retlis lis1))
      ((< (car lis1) (car lis2)) (cons (car lis1) (pre_sortedmerge (cdr lis1) lis2 retlis)))
      (else (cons (car lis2) (pre_sortedmerge lis1 (cdr lis2) retlis)))))

(define (sortedmerge lis1 lis2) (pre_sortedmerge lis1 lis2 '()))

; 5.6 function interleave
(define (pre_interleave lis1 lis2 retlis num)
  (cond
    ((null? lis1) (merge retlis lis2))
    ((null? lis2) (merge retlis lis1))
    ((= (remainder num 2) 0) (let ((temp (+ num 1)))
                 (cons (car lis1) (pre_interleave (cdr lis1) lis2 retlis temp))))
    ((= (remainder num 2) 1) (let ((temp (+ num 1)))
                 (cons (car lis2) (pre_interleave lis1 (cdr lis2) retlis temp))))))

(define (interleave lis1 lis2) (pre_interleave lis1 lis2 '() 0))

; 5.7 function map2
(define (checklen lis num)
  (cond
    ((null? lis) num)
    (else (+ 1 (checklen (cdr lis) num)))))

(define (pre_map2 jlis llis p f retlis)
  (cond
    ((null? jlis) retlis)
    ((= (checklen jlis 0) (checklen llis 0)) (cond
                                               ((p (car jlis)) (cons (f (car llis)) (pre_map2 (cdr jlis) (cdr llis) p f retlis)))
                                               (else (cons (car llis) (pre_map2 (cdr jlis) (cdr llis) p f retlis)))))
    (else (display "Error: provided lists aren't of the same size"))))

(define (map2 jlis llis p f) (pre_map2 jlis llis p f '()))

; 5.8.a function elisTOalis (edge lis to adj lis)

(define (idkwiadt caplis endlis)
  (cond
    ((null? caplis) endlis)
    (else (idkwiadt (cdr caplis) (cons (car caplis) endlis)))))

(define (add lis redlis caplis)
  (cond
    ((null? redlis) (idkwiadt caplis (cons (zip (cons (car lis) '()) (cdr lis)) '())))
    ((eq? (car (car (car redlis))) (car lis)) (idkwiadt caplis (zip (zip (car (car redlis)) (cons (car (cdr lis)) (car (cdr (car redlis))))) (car (cdr redlis)))))
    (else (add lis (cdr redlis) (cons (car redlis) caplis)))))

(define (pre_elisTOalis elis alis)
  (cond
    ((null? elis) alis)
    (else (pre_elisTOalis (cdr elis) (add (car elis) alis '())))))

(define (lonenode alis lis)
  (cond
    ((null? alis) (zip (cdr lis) '()))
    ((eq? (car (cdr lis)) (car (car (car alis)))) '*)
    (else (lonenode (cdr alis) lis))))

(define (forall alis elis)
  (cond
    ((null? elis) alis)
    (else (let ((ret (lonenode alis (car elis))))
            (cond
            ((symbol? ret) (forall alis (cdr elis)))
            (else (forall (cons ret alis) (cdr elis))))))))

(define (elisTOalis elis) (forall (pre_elisTOalis elis '()) elis))

; 5.8.b function reverse_elisTOalis (adj lis to edge lis)

(define (smallu adj retlis)
  (cond
    ((null? (car (cdr adj))) retlis)
    (else (smallu (zip (car adj) (cdr (car (cdr adj)))) (cons (zip (car (car adj)) (car (car (cdr adj)))) retlis))) ))

(define (pre_reverse_elisTOalis alis retlis)
  (cond
  ((null? alis) retlis)
  (else (pre_reverse_elisTOalis (cdr alis) (smallu (car alis) retlis)))))

(define (reverse_elisTOalis alis) (pre_reverse_elisTOalis alis '()))