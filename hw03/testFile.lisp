(defvar filename "test.lisp")

(load filename)

(format t "(root1 1 5 1) returns ~a~%" (root1 1 5 1))
(format t "(root2 1 5 1) returns ~a~%" (root2 1 5 1))