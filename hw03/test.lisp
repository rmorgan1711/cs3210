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
    (find-best-recurse velocity elevation 0 0)
)

(defun find-best-recurse (velocity elevation angle currBest)
    (setq d0 (travel-distance-simple elevation velocity angle))
    (setq d1 (travel-distance-simple elevation velocity (+ 1 angle)))
    (setq dcurrBest (travel-distance-simple elevation velocity currBest))
    (setq dBest (max d0 d1 dcurrBest))

    (cond
        ((>= angle 91) currBest)
        ((= dBest dcurrBest) (find-best-recurse velocity elevation (+ 1 angle) currBest))
        ((= dBest d1) (find-best-recurse velocity elevation (+ 1 angle) (+ 1 angle)))
        ((= dBest d0) (find-best-recurse velocity elevation (+ 1 angle) angle))        
    )
)