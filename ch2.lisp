; Concrete Abstractions (Hailperin, Kaiser, Knight), 1999
;
; Chapter 2
;
; Note: Exercises from Concrete Abtractions are written in Scheme in the text,
; but have been rewritten in Common Lisp for the sake of going through both
; books concurrently
;

; Multiplication in terms of addition in Lisp
; 2 * 3
; 2 + (2 * 2)
; 4 + (2 * 1)
; 6 + (2 * 0)

(defun mult (a b)
	(cond ((eq a 0) 0) 
		  ((eq b 0) 0)
		  ((< a 0) (- 0 (mult (- 0 a) b))) 
		  ((< b 0) (- 0 (mult a (- 0 b))))
		  (t (+ a (mult a (- b 1))))))

; Division in terms of subtraction
; ((< a b) 0) part of (cond) not necessary after handling negatives
; 10 / 2
; 1 + ((10 - 2) / 2)
; 2 + ((8 - 2) / 2)
; 3 + ((6 - 2) / 2)
; 4 + ((4 - 2) / 2)
; 5 + ((2 - 2) / 2)
; (same-as) 5 + (0 / 2)
; (same-as) 5 + 0

(defun quot (a b)
	(cond ((eq a 0) 0)
		  ((eq b 0) 0)
		  ((< a 0) (- 0 (quot (- 0 a) b)))
		  ((< b 0) (- 0 (quot a (- 0 b))))
		  (t (+ 1 (quot (- a b) b)))))

; Factorial in Lisp
; (factorial 5)
; (* 5 (factorial 4))
; (* 20 (factorial 3))
; (* 60 (factorial 2))
; (* 120 (factorial 1))
; (* 120 (factorial 0))
; (* 120 1)
; (120)

(defun factorial (n)
	(if (eq n 0) 1
		(* n (factorial (- n 1)))))

; OR

(defun factorial2 (n)
	(if (eq n 1) 1
		(* n (factorial2 (- n 1)))))

; Sum of first n numbers, similar to factorial but with addition instead of multiplication
; Written before looking at solution in book, which starats with base case (= n 1)
(defun sum-of-first (n)
	(if (eq n 0) 0
		(+ n (sum-of-first (- n 1)))))

; Sum of first with reverse ordering - value remains the same
(defun sum-of-first (n)
	(if (eq n 0) 0
		(+ (sum-of-first (- n 1)) n)))

; Subtract n from n - 1 until n = 0
(defun subtract-the-first (n)
	(if (= n 0) 0
		(- (subtract-the-first (- n 1)) n)))

; Subtract n - 1 from n until n = 0
(defun subtract-the-first (n)
	(if (= n 0) 0
		(- n (subtract-the-first (- n 1)))))

(defun sum-of-integers (low high)
	(if (> low high) 0
		(+ (sum-of-integers low (- high 1))
			high)))

; Alternative to (expt n 2)
(defun square (n)
	(if (eq n 0) 0
		(* n n)))

(defun sum-of-squares (n)
	(if (eq n 0) 0
		(+ (expt n 2) (sum-of-squares (- n 1)))))

(defun sum-of-cubes (n)
	(if (eq n 0) 0
		(+ (expt n 3) (sum-of-squares (- n 1)))))

(defun power (a b)
	(cond ((eq a 0) 0)
		  ((eq b 0) 1)
		  (t (* a (power a (- b 1)))))) ; Always have t as last test in cond

(defun sum-of-powers (a b)
	(cond ((eq a 0) 0)
		  ((eq b 0) 1)
		  (t (+ (power a b) (sum-of-powers (- a 1) b)))))

; Divide by 10 to determine how many digits, no need to 
; account for 0 since it is handled by (< n 10)
(defun num-digits (n)
	(cond ((< n 10) 1)
		  (t (1+ (num-digits (/ n 10))))))

; num-digits extended to handle negatives
(defun num-digits (n)
	(cond ((< n 0) (num-digits (- n)))
		  ((< n 10) 1)
		  (t (1+ (num-digits (/ n 10))))))

; Count sixes in a number
(defun num-sixes (n)
	(cond ((eq n 6) 1)
		  ((< n 10) 0) 
		  ((eq (mod n 10) 6) (1+ (num-sixes (/ (- n 6) 10))))
		  (t (num-sixes (/ (- n (mod n 10)) 10)))))

; Cound digit d in a number
(defparameter *current-digit* nil)
(defun num-digit (n d)
	(cond ((eq n d) 1)
		  ((< n 10) 0) 
		  ((eq (mod n 10) d) (setf *current-digit* (mod n 10))
		  				     (1+ (num-digit (/ (- n d) 10) d)))
		  (t 				 (setf *current-digit* (mod n 10))
							 (num-digit (/ (- n *current-digit*) 10) d))))

; Use global variable to store current digit, use to sum digits
; Note: This approach could be used for prior problems as well
; to avoid duplication
(defparameter *current-digit* nil)
(defun sum-digits (n)
	(cond ((eq n 0) 0)
		  ((< n 10) n)
		  (t (setf *current-digit* (mod n 10))
		  	 (+ *current-digit* (sum-digits (/ (- n *current-digit*) 10))))))

; Exercise 2.12
(defun find-exp-2 (n)
	(cond ((oddp n) 0)
		  (t (1+ (find-exp-2 (/ n 2))))))

; Exercise 2.16
; Use induction to prove that (foo x n) terminates with the value
;
; x^(n + 1) - 1
; -------------
; 	  x - 1
;

(defun foo (x n)
	(if (eq n 0) 0
		(+ (expt x n) (foo x (- n 1)))))

; Base case: (foo x 0) terminates with the value 1 because of the evaluation rule if. 
; Becuase... 
;
; x^(1) - 1 
; ---------
;   x - 1 
;
; is the same as (x - 1) / (x - 1), which is the same as 1, (foo x 0) computes the correct value.
;
; Induction hypothesis: Assume that (foo x k) terminates with the value of (x^(k + 1) -1) / (x - 1) for all
; k in the range 0 ≤ k < n.
;
; Inductive step: Consider evaluating (foo x n), with n > 0. This will terminate if the evaluation of
; (foo x (- n 1)) terminates and will have the same value as (+ (expt x n) (foo x (- n 1))). Because (- n 1)
; evaluates to n - 1, and 0 ≤ n - 1 < n, we can therefore assume by our induction hypothesis that 
; (foo x (- n 1)) does terminate, with the value of x^((n - 1) + 1) - 1 / x - 1, which simplifies to
; (x^n - 1) / x - 1. Because...
;
; ((x^((n-1) + 1) - 1) / x - 1) + x^n 
; = (x^n - 1 / x - 1) + x^n
; = (x^n - 1 / x - 1) + (x-1)x^n / x - 1
; = (x^n - 1 / x - 1) + (x^(n + 1) - x^n / x - 1)
; = (x^n - 1 + x^(n + 1) - x^n) / x - 1
; = (x^n - x^n + x^(n + 1) - 1) / x - 1
; = (x^(n + 1) - 1) / x - 1
; ...we see that (foo x n) does terminate with the correct value for any arbitrary positive n, under the 
; inductive hypothesis of correct operation for smaller arguments.


; Exercise 2.17
;
; Perhaps you have heard the following Christmas song:
;
; On the first day of Christmas My true love gave to me
; A partridge in a pear tree.
;
; On the second day of Christmas My true love gave to me
; Two turtle doves
; And a partridge in a pear tree.
;
; On the third day of Christmas My true love gave to me Three French hens,
; Two turtle doves,
; And a partridge in a pear tree.
;
; Given:

(defun presents-on-day (n)
	(if (eq n 1) 1
		(+ n (presents-on-day (- n 1)))))

; Write procedure (presents-through-day n), using (presents-on-day n)

(defun presents-through-day (n)
	(if (eq n 1) 1
		(+ (presents-on-day n) (presents-through-day (- n 1)))))


; --------------------------------------------------------------------------------------------------------

; Exercise 2.18
; Prove by induction that for every nonnegative integer n the following procedure computes 2n:
  
(defun f (n)
	(if (eq n 0) 0
		(+ 2 (f (- n 1)))))

; Base case: (f 0) terminates with the value of 0 because of the evaluation rule if.
; Because 0 = 2(0), (f 0) computes the correct value.
;
; Induction hypothesis: Assume that (f k) terminates with the value 2k for all k in the range 0 ≤ k < n
;
; Inductive step: Consider evaluating (f n), with n > 0. This will terminate if 
; the evaluation of (f (- n 1)) terminates and will have the same value as (+ 2 (f (- n 1))). Becuase 
; (- n 1) evaluates to n - 1, and 0 ≤ n - 1 < n, we can therefore assume by our induction hypothesis that
; (f (- n 1)) does terminate with the value 2n. Therefore, (+ 2 (f (- n 1))) evaluates to 2 + 2(n - 1). 
; Becuase ...
; 2n = 2 + 2(n - 1)
; 2n = 2 + 2n - 2
; 2n = 2n
; ...we see that (f n) does terminate with the correct value for any arbitrary positive n, under the inductive
; hypothesis of correct operation for smaller arguments.
;
; Conclusion: Therefore, by mathematical induction on n, (f n) terminates with the value 2n for any
; nonnegative integer n.