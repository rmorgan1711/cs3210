(defvar filename "test.lisp")
(load filename)

; (write-line "Position tests")
; (format t "(pos 0 0 0 0) returns ~a~%" (pos 0 0 0 0))
; (format t "(pos 0 0 20 20) returns ~a~%" (pos 0 0 20 20))
; (format t "(pos 0 5 10 10) returns ~a~%" (pos 0 5 10 10))
; (format t "(pos 2 2 2 2) returns ~a~%" (pos 2 2 2 2))
; (format t "(pos 5 5 5 5) returns ~a~%" (pos 5 5 5 5))

; (write-line "----------------")

; (write-line "Roots tests")
; (format t "(root1 1 5 1) returns ~a~%" (root1 1 5 1))
; (format t "(root2 1 5 1) returns ~a~%" (root2 1 5 1))
; (format t "(root1 3 5 3) returns ~a~%" (root1 3 5 3))
; (format t "(root2 3 5 3) returns ~a~%" (root2 3 5 3))
; (format t "(root1 1 1 0) returns ~a~%" (root1 1 1 0))
; (format t "(root2 1 1 0) returns ~a~%" (root2 1 1 0))
; (format t "(root1 1 2 -8) returns ~a~%" (root1 1 2 (- 0 8)))
; (format t "(root2 1 2 -8) returns ~a~%" (root2 1 2 (- 0 8)))
; (format t "(root1 3 1 5) returns ~a~%" (root1 3 1 5))
; (format t "(root2 3 1 5) returns ~a~%" (root2 3 1 5))

; (write-line "----------------")

; (write-line "Test time-to-impact")
; (format t "v = 5    h = 1:  ~a~%" (time-to-impact 5 1))
; (format t "v = 0    h = 1:  ~a~%" (time-to-impact 0 1))
; (format t "v = 0    h = 10: ~a~%" (time-to-impact 0 10))
; (format t "v = 100  h = 10: ~a~%" (time-to-impact 100 10))
; (format t "v = 100  h = 0:  ~a~%" (time-to-impact 100 0))
; (format t "v = 0    h = 0:  ~a~%" (time-to-impact 0 0))

; (write-line "----------------")

; (write-line "Test time-to-height")
; (format t "v = 5    h = 1:  target = 0.5:  ~a~%" (time-to-height 5 1 0.5))
; (format t "v = 0    h = 1:  target = 1:    ~a~%" (time-to-height 0 1 1))
; (format t "v = 0    h = 10: target = 5:    ~a~%" (time-to-height 0 10 5))
; (format t "v = 100  h = 10: target = 5:    ~a~%" (time-to-height 100 10 5))
; (format t "v = 100  h = 0:  target = 50:   ~a~%" (time-to-height 100 0 50))
; (format t "v = 0    h = 0:  target = 0:    ~a~%" (time-to-height 0 0 0))

; (write-line "----------------")

; (write-line "Test travel-distance-simple")
; (format t "~a~%" (travel-distance-simple 1 45 0))
; (format t "~a~%" (travel-distance-simple 1 45 45))
; (format t "~a~%" (travel-distance-simple 1 45 90))

(write-line "----------------")

(write-line "Test find-best-angle")
(format t "~a~%" (find-best-angle 45 1))