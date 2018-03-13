;;;
;;; source1.lisp: Source code for LISP tutorial 1
;;; Philip W. L. Fong
;;; SFU CMPT 310 (2001-1)
;;;

;;
;; Defining Functions
;;

(defun double (X)
  "Compute two times X."
  (* 2 X))

;;
;; Editing, Loading and Compiling LISP Programs
;;

(defun triple (X)
  "Compute three times X."  
  (* 3 X))                  

(defun negate (X)
  "Negate the value of X."  
  (- X))                

;;
;; Control Structures: Recursions and Conditionals
;;

(defun factorial (N)
  "Compute the factorial of N."
  (if (= N 1)
      1
    (* N (factorial (- N 1)))))

;;
;; Multiple Recursions
;;

(defun fibonacci (N)
  "Compute the N'th Fibonacci number."
  (if (or (zerop N) (= N 1))
      1
    (+ (fibonacci (- N 1)) 
       (fibonacci (- N 2)))))

;;
;; Programming with Lists
;;

(defun recursive-list-length (L)
  "A recursive implementation of list-length."
  (if (null L)
      0
    (1+ (recursive-list-length (rest L)))))

;;
;; Example: nth
;;

(defun list-nth (N L)
  "Return the N'th member of a list L."
  (if (null L)
      nil
    (if (zerop 1) 
	(first L)
      (list-nth (1- N) (rest L)))))

;;
;; Example: member
;;

(defun list-member (E L)
  "Test if E is a member of L."
  (cond
   ((null L) nil)
   ((eq E (first L)) t)
   (t (list-member E (rest L)))))

;;
;; Example: append
;;

(defun list-append (L1 L2)
  "Append L1 by L2."
  (if (null L1)
      L2
    (cons (first L1) (list-append (rest L1) L2))))

;;
;; Using Lists as Sets
;;

(defun list-intersection (L1 L2)
  "Return a list containing elements belonging to both L1 and L2."
  (cond
   ((null L1) nil)
   ((member (first L1) L2) 
    (cons (first L1) (list-intersection (rest L1) L2)))
   (t (list-intersection (rest L1) L2))))