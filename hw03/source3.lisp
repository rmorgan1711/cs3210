;;;
;;; source3.lisp: Source code for LISP Tutorial 3
;;; Philip W. L. Fong
;;; SFU CMPT 310 (2001-1)
;;;

;;
;; Binary Trees
;;

;;
;; Constructors for binary trees
;;

(defun make-bin-tree-leaf (E)
  "Create a leaf."
  (list E))

(defun make-bin-tree-node (E B1 B2)
  "Create a node with element K, left subtree B1 and right subtree B2."
  (list E B1 B2))

;;
;; Selectors for binary trees
;;

(defun bin-tree-leaf-element (L)
  "Retrieve the element of a leaf L."
  (first L))

(defun bin-tree-node-element (N)
  "Retrieve the element of a node N."
  (first N))

(defun bin-tree-node-left (N)
  "Retrieve the left subtree of a node N."
  (second N))

(defun bin-tree-node-right (N)
  "Retrieve the right subtree of a node N."
  (third N))

;;
;; Recognizers for binary trees
;;

(defun bin-tree-leaf-p (B)
  "Test if binary tree B is a leaf."
  (and (listp B) (= (list-length B) 1)))

(defun bin-tree-node-p (B)
  "Test if binary tree B is a node."
  (and (listp B) (= (list-length B) 3)))

;;
;; Binary Tree Operations
;;

(defun bin-tree-member-p (B E)
  "Test if E is an element in binary tree B."
  (if (bin-tree-leaf-p B)
      (equal E (bin-tree-leaf-element B))
    (let
	((elmt  (bin-tree-node-element B))
	 (left  (bin-tree-node-left    B))
	 (right (bin-tree-node-right   B)))
      (or (equal E elmt)
	  (bin-tree-member-p left E)
	  (bin-tree-member-p right E)))))

(defun bin-tree-reverse (B)
  "Reverse binary tree B."
  (if (bin-tree-leaf-p B)
      B
    (let
	((elmt  (bin-tree-node-element B))
	 (left  (bin-tree-node-left    B))
	 (right (bin-tree-node-right   B)))
      (make-bin-tree-node elmt
			  (bin-tree-reverse right)
			  (bin-tree-reverse left)))))

(defun bin-tree-preorder (B)
  "Create a list containing keys of B in preorder."
  (if (bin-tree-leaf-p B)
      (list (bin-tree-leaf-element B))
    (let
	((elmt  (bin-tree-node-element B))
	 (left  (bin-tree-node-left    B))
	 (right (bin-tree-node-right   B)))
      (cons elmt
	    (append (bin-tree-preorder left)
		    (bin-tree-preorder right))))))

(defun fast-bin-tree-preorder (B)
  "A tail-recursive version of bin-tree-preorder."
  (preorder-aux B nil))

(defun preorder-aux (B A)
  "Append A to the end of the list containing elements of B in preorder."
  (if (bin-tree-leaf-p B)
      (cons (bin-tree-leaf-element B) A)
    (let
	((elmt  (bin-tree-node-element B))
	 (left  (bin-tree-node-left    B))
	 (right (bin-tree-node-right   B)))
      (cons elmt
	    (preorder-aux left
			  (preorder-aux right A))))))

;;
;; Set
;;

(defun make-empty-set ()
  "Creates an empty set."
  nil)

(defun set-insert (S E)
  "Return a set containing all the members of set S plus the element E."
  (adjoin E S :test #'equal))

(defun set-remove (S E)
  "Return a set containing all the members of set S except for element E."
  (remove E S :test #'equal))

(defun set-member-p (S E)
  "Return non-NIL if set S contains element E."
  (member E S :test #'equal))

(defun set-empty-p (S)
  "Return true if set S is empty."
  (null S))

;;
;; Binary Search Trees (BST)
;;

(defun make-empty-BST ()
  "Create an empty BST."
  nil)

(defun BST-empty-p (B)
  "Check if BST B is empty."
  (null B))

(defun BST-member-p (B E)
  "Check if E is a member of BST B."
  (if (BST-empty-p B)
      nil
    (BST-nonempty-member-p B E)))

(defun BST-nonempty-member-p (B E)
  "Check if E is a member of nonempty BST B."
  (if (bin-tree-leaf-p B)
      (= E (bin-tree-leaf-element B))
    (if (<= E (bin-tree-node-element B))
	(BST-nonempty-member-p (bin-tree-node-left B) E)
      (BST-nonempty-member-p (bin-tree-node-right B) E))))

(defun BST-insert (B E)
  "Insert E into BST B."
  (if (BST-empty-p B)
      (make-bin-tree-leaf E)
    (BST-nonempty-insert B E)))

(defun BST-nonempty-insert (B E)
  "Insert E into nonempty BST B."
  (if (bin-tree-leaf-p B)
      (BST-leaf-insert B E)
    (let ((elmt  (bin-tree-node-element B))
	  (left  (bin-tree-node-left    B))
	  (right (bin-tree-node-right   B)))
      (if (<= E (bin-tree-node-element B))
	  (make-bin-tree-node elmt
			      (BST-nonempty-insert (bin-tree-node-left B) E)
			      right)
	(make-bin-tree-node elmt
			    left
			    (BST-nonempty-insert (bin-tree-node-right B) E))))))

(defun BST-leaf-insert (L E)
  "Insert element E to a BST with only one leaf."
  (let ((elmt (bin-tree-leaf-element L)))
    (if (= E elmt)
	L
      (if (< E elmt)
	  (make-bin-tree-node E
			      (make-bin-tree-leaf E)
			      (make-bin-tree-leaf elmt))
	(make-bin-tree-node elmt
			    (make-bin-tree-leaf elmt)
			    (make-bin-tree-leaf E))))))

(defun BST-remove (B E)
  "Remove E from BST B."
  (if (BST-empty-p B)
      B
    (if (bin-tree-leaf-p B)
	(BST-leaf-remove B E)
      (BST-node-remove B E))))

(defun BST-leaf-remove (L E)
  "Remove E from BST leaf L."
  (if (= E (bin-tree-leaf-element L))
      (make-empty-BST)
    L))

(defun BST-node-remove (N E)
  "Remove E from BST node N."
  (let
      ((elmt  (bin-tree-node-element N))
       (left  (bin-tree-node-left    N))
       (right (bin-tree-node-right   N)))
    (if (<= E elmt)
	(if (bin-tree-leaf-p left)
	    (if (= E (bin-tree-leaf-element left))
		right
	      N)
	  (make-bin-tree-node elmt (BST-node-remove left E) right))
      (if (bin-tree-leaf-p right)
	  (if (= E (bin-tree-leaf-element right))
	      left
	    N)
	(make-bin-tree-node elmt left (BST-node-remove right E))))))

;;
;; Polynomials
;;

;;
;; Constructors for polynomials
;;

(defun make-constant (num)
  num)

(defun make-variable (sym)
  sym)

(defun make-sum (poly1 poly2)
  (list '+ poly1 poly2))

(defun make-product (poly1 poly2)
  (list '* poly1 poly2))

(defun make-power (poly num)
  (list '** poly num))

;;
;; Recognizers for polynomials
;;

(defun constant-p (poly)
  (numberp poly))

(defun variable-p (poly)
  (symbolp poly))

(defun sum-p (poly)
  (and (listp poly) (eq (first poly) '+)))

(defun product-p (poly)
  (and (listp poly) (eq (first poly) '*)))

(defun power-p (poly)
  (and (listp poly) (eq (first poly) '**)))

;;
;; Selectors for polynomials
;;

(defun constant-numeric (const)
  const)

(defun variable-symbol (var)
  var)

(defun sum-arg1 (sum)
  (second sum))

(defun sum-arg2 (sum)
  (third sum))

(defun product-arg1 (prod)
  (second prod))

(defun product-arg2 (prod)
  (third prod))

(defun power-base (pow)
  (second pow))

(defun power-exponent (pow)
  (third pow))

;;
;; Unevaluated derivative
;;

(defun make-derivative (poly x)
    (list 'd poly x))

(defun derivative-p (poly)
  (and (listp poly) (eq (first poly) 'd)))

;;
;; Differentiation function
;;

(defun d (poly x)
  (cond
   ((constant-p poly) 0)
   ((variable-p poly) 
    (if (equal poly x) 
	1 
      (make-derivative poly x)))
   ((sum-p poly) 
    (make-sum (d (sum-arg1 poly) x) 
	      (d (sum-arg2 poly) x)))
   ((product-p poly) 
    (make-sum (make-product (product-arg1 poly) 
			    (d (product-arg2 poly) x))
	      (make-product (product-arg2 poly) 
			    (d (product-arg1 poly) x))))
   ((power-p poly)
    (make-product (make-product (power-exponent poly)
				(make-power (power-base poly) 
					    (1- (power-exponent poly))))
		  (d (power-base poly) x)))))

;;
;; Simplification function
;;

(defun simplify (poly)
  "Simplify polynomial POLY."
  (cond
   ((constant-p poly) poly)
   ((variable-p poly) poly)
   ((sum-p poly)
    (let ((arg1 (simplify (sum-arg1 poly)))
	  (arg2 (simplify (sum-arg2 poly))))
      (make-simplified-sum arg1 arg2)))
   ((product-p poly)
    (let ((arg1 (simplify (product-arg1 poly)))
	  (arg2 (simplify (product-arg2 poly))))
      (make-simplified-product arg1 arg2)))
   ((power-p poly)
    (let ((base (simplify (power-base poly)))
	  (exponent (simplify (power-exponent poly))))
      (make-simplified-power base exponent)))
   ((derivative-p poly) poly)))

(defun make-simplified-sum (arg1 arg2)
  "Given simplified polynomials ARG1 and ARG2, construct a simplified sum of ARG1 and ARG2."
  (cond
   ((and (constant-p arg1) (zerop arg1)) arg2)
   ((and (constant-p arg2) (zerop arg2)) arg1)
   (t                                    (make-sum arg1 arg2))))

(defun make-simplified-product (arg1 arg2)
  "Given simplified polynomials ARG1 and ARG2, construct a simplified product of ARG1 and ARG2."
  (cond
   ((and (constant-p arg1) (zerop arg1)) (make-constant 0))
   ((and (constant-p arg2) (zerop arg2)) (make-constant 0))
   ((and (constant-p arg1) (= arg1 1))   arg2)
   ((and (constant-p arg2) (= arg2 1))   arg1)
   (t                                    (make-product arg1 arg2))))

(defun make-simplified-power (base exponent)
  "Given simplified polynomials BASE and EXPONENT, construct a simplified power with base BASE and exponent EXPONENT."
  (cond
   ((and (constant-p exponent) (= exponent 1))   base)
   ((and (constant-p exponent) (zerop exponent)) (make-constant 1))
   (t                          (make-power base exponent))))

;;
;; Tower of Hanoi
;;

;;
;; A tower is a list of numbers
;;

(defun make-empty-tower ()
  "Create tower with no disk."
  nil)

(defun tower-push (tower disk)
  "Create tower by stacking DISK on top of TOWER."
  (cons disk tower))

(defun tower-top (tower)
  "Get the top disk of TOWER."
  (first tower))

(defun tower-pop (tower)
  "Remove the top disk of TOWER."
  (rest tower))

;;
;; Hanoi configuration
;;

(defun make-hanoi (from-tower aux-tower to-tower)
  "Create a Hanoi configuration from three towers."
  (list from-tower aux-tower to-tower))

(defun hanoi-tower (hanoi i)
  "Select the I'th tower of a Hanoi construction."
  (nth (1- i) hanoi))

;;
;; Utilities
;;

(defun hanoi-tower-update (hanoi i tower)
  "Replace the I'th tower in the HANOI configuration by tower TOWER."
  (cond
   ((= i 1) (make-hanoi tower (second hanoi) (third hanoi)))
   ((= i 2) (make-hanoi (first hanoi) tower (third hanoi)))
   ((= i 3) (make-hanoi (first hanoi) (second hanoi) tower))))

(defun hanoi-tower-top (hanoi i)
  "Return the top disk of the I'th tower in the HANOI configuration."
  (tower-top (hanoi-tower hanoi i)))

(defun hanoi-tower-pop (hanoi i)
  "Pop the top disk of the I'th tower in the HANOI configuration."
  (hanoi-tower-update hanoi i (tower-pop (hanoi-tower hanoi i))))

(defun hanoi-tower-push (hanoi i disk)
  "Push DISK into the I'th tower of the HANOI configuration."
  (hanoi-tower-update hanoi i (tower-push (hanoi-tower hanoi i) disk)))

;;
;; Operator: move top disk from one tower to another
;;

(defun move-disk (from to hanoi)
  "Move the top disk from peg FROM to peg TO in configuration HANOI."
  (let
      ((disk               (hanoi-tower-top hanoi from))
       (intermediate-hanoi (hanoi-tower-pop hanoi from)))
    (hanoi-tower-push intermediate-hanoi to disk)))

;;
;; Subgoal: moving a tower from one peg to another
;;

(defun move-tower (N from aux to hanoi)
  "In the HANOI configuration, move the top N disks from peg FROM to peg TO using peg AUX as an auxiliary peg."
  (if (= N 1)
      (move-disk from to hanoi)
    (move-tower (- N 1) aux from to 
		(move-disk from to
			   (move-tower (- N 1) from to aux hanoi)))))

;;
;; Driver function
;;

(defun solve-hanoi (N)
  "Solve the Tower of Hanoi problem."
  (move-tower N 1 2 3 (make-hanoi (make-complete-tower N) nil nil)))

(defun make-complete-tower (N)
  "Create a tower of N disks."
  (make-complete-tower-aux N (make-empty-tower)))

(defun make-complete-tower-aux (N A)
  "Push a complete tower of N disks on top of tower A."
  (if (zerop N)
      A
    (make-complete-tower-aux (1- N) (tower-push A N))))
