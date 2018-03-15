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