;;;
;;; source2.lisp: Source code for LISP tutorial 2
;;; Philip W. L. Fong
;;; SFU CMPT 310 (2001-1)
;;;

;;
;; Auxiliary Functions and Accumulator Variables
;;

(defun list-append (L1 L2)
  "Append L1 by L2."
  (if (null L1)
      L2
    (cons (first L1) (list-append (rest L1) L2))))

(defun slow-list-reverse (L)
  "Create a new list containing the elements of L in reversed order."
  (if (null L)
      nil
    (list-append (slow-list-reverse (rest L)) 
		 (list (first L)))))

(defun list-reverse (L)
  "Create a new list containing the elements of L in reversed order."
  (list-reverse-aux L nil))

(defun list-reverse-aux (L A)
  "Append the reversal of list L to list A."
  (if (null L)
      A
    (list-reverse-aux (rest L) (cons (first L) A))))

;;
;; Factorial Revisited
;;

(defun fast-factorial (N)
  "A tail-recursive version of factorial."
  (fast-factorial-aux N 1))

(defun fast-factorial-aux (N A)
  "Multiply A by the factorial of N."
  (if (= N 1)
      A
    (fast-factorial-aux (- N 1) (* N A))))

;;
;; Tail Recursions
;;

(defun factorial (N)
  "Compute the factorial of N."
  (if (= N 1)
      1
    (* N (factorial (- N 1)))))


;;
;; Functions as First-Class Objects
;;

(defun double (x)
  "Multiple X by 2."
  (* 2 x))

(defun repeat-transformation (F N X)
  "Repeat applying function F on object X for N times."
  (if (zerop N)
      X
    (repeat-transformation F (1- N) (funcall F X))))

;;
;; Higher-Order Functions
;;

(defun prepend-blah (L) 
  "Add 'BLAH to the front of a list L."
  (cons 'blah L))

(defun list-nth (N L)
  "Return the N'th member of list L."
  (first (repeat-transformation (function rest) N L)))

;;
;; Iterating Through a List
;;

(defun double-list-elements (L)
  "Given a list L of numbers, return a list containing the elements of L multiplied by 2."
  (if (null L)
      nil
    (cons (double (first L)) (double-list-elements (rest L)))))

(defun reverse-list-elements (L)
  "Given a list L of lists, return a list containing the reversal of L's members."
  (if (null L)
      nil
    (cons (reverse (first L)) (reverse-list-elements (rest L)))))

(defun mapfirst (F L)
  "Apply function F to every element of list L, and return a list containing the results."
  (if (null L)
      nil
    (cons (funcall F (first L)) (mapfirst F (rest L)))))

;;
;; Search Iteration
;;

(defun find-even (L)
  "Given a list L of numbers, return the leftmost even member."
  (if (null L)
      nil
    (if (evenp (first L))
        (first L)
      (find-even (rest L)))))

(defun list-find-if (P L)
  "Find the leftmost element of list L that satisfies predicate P."
  (if (null L)
      nil
    (if (funcall P (first L))
        (first L)
      (list-find-if P (rest L)))))

;;
;; Filter Iteration
;;

(defun remove-short-lists (L)
  "Remove all members of L that has length less than three."
  (if (null L)
      nil
    (if (< (list-length (first L)) 3)
        (remove-short-lists (rest L))
      (cons (first L) (remove-short-lists (rest L))))))

(defun remove-even (L)
  "Remove all members of L that is an even number."
  (if (null L)
      nil
    (if (zerop (rem (first L) 2))
        (remove-even (rest L))
      (cons (first L) (remove-even (rest L))))))

(defun list-intersection (L1 L2)
  "Compute the intersection of lists L1 and L2."
  (remove-if #'(lambda (X) (not (member X L2))) L1))

;;
;; Functions Returning Multiple Values
;;

(defun order (a b)
  "Return two values: (min a b) and (max a b)."
  (if (>= a b)
      (values b a)
    (values a b)))