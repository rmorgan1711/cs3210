(defun pos (a v u tm)
    (+ (* .5 a tm tm) (* v tm) u)
)

(defun root1 (a b c)
    (setq test (- (* b b) (* 4 a c)))
    (if (>= test 0)
        (/ (- (- 0 b) (sqrt test)) (* 2 a))
        ()
    )
)

(defun root2 (a b c)
    (setq test (- (* b b) (* 4 a c)))
    (if (>= test 0)
        (/ (+ (- 0 b) (sqrt test)) (* 2 a))
        ()
    )
)

(defconstant GRAVITY 9.8)

(defun time-to-impact (vertical-velocity elevation)
    (setq a (* 0.5 GRAVITY (- 1)))
    (setq r1 (root1 a vertical-velocity elevation))
    (setq r2 (root2 a vertical-velocity elevation))

    (if (>= r1 0) r1 r2)
)

(defun time-to-height (vertical-velocity elevation target-elevation)
    (time-to-impact vertical-velocity (- elevation target-elevation))
)

(defun degree2radian (deg)
    (/ (*  deg pi) 180.)
)

(defun travel-distance-simple (elevation velocity angle)
    (setq vx (* velocity (cos (degree2radian angle))))
    (setq vy (* velocity (sin (degree2radian angle))))
    (setq tf (time-to-impact vy elevation))
    (* vx tf)
)

(defun find-best-angle (velocity elevation)
    (setq startDeg 0)
    (setq a0 (travel-distance-simple elevation velocity startDeg))
    (setq a1 (travel-distance-simple elevation velocity (+ 1 startDeg)))
    
)

(defun asdf (velocity elevation angle)
    (setq a0 (travel-distance-simple elevation velocity angle))
    (setq a1 (travel-distance-simple elevation velocity (+ 1 angle)))

    (format t "~a~%" (list angle a0 a1))

    (cond
        ((= angle 89)
            (if (= a0 (max a0 a1)) angle (+ 1 angle))
        )
        ((> a1 a0)
            (setq a3 (asdf velocity elevation (+ 1 angle)))
            ; (format t "~a~%" (list a1))
            (if (> a3 a1) 
                (format t "~a~%" (list (+ 1 angle) angle))
                (format t "~a~%" (list angle (+ 1 angle)))
            )

            (if (> a3 a1) (+ 1 angle) angle)
        )
        (t
            (setq a3 (asdf velocity elevation (+ 1 angle)))
            ; (format t "~a~%" (list a0))
            (if (> a3 a0) 
                (format t "~a~%" (list (+ 1 angle) angle))
                (format t "~a~%" (list angle (+ 1 angle)))
            )

            (if (> a3 a0) (+ 1 angle) angle)
        )
    )
)