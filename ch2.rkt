; Chapter 2

(define square
	(lambda (n)
		(if (= n 0)
		0
		(if (even? n)
			(+ (square (/ n 2))
				(* 3 (/ n 2)))
			(+ (square (- n 1))
				(- (+ n n) 1))))))

(define mult
	(lambda (a b)
		(cond ((or (= a 0) (= b 0)) 0)
			  ((< a 0) (- (mult ((- a) b))))
			  ((< b 0) (- (mult (a ( - b)))))
			  (else (+ a (mult a (- b 1)))))))