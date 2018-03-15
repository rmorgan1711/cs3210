;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v 
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a 
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2 
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when 
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(defun square (x)
  (* x x)
)

;; these are constants that will be useful to us
;(defun gravity 9.8)  ;; in m/s
;(defun pi 3.14159)

(defconstant GRAVITY 9.8)   ;; in m/s
(defconstant PI 3.14159)

;; Problem 1

(defun pos (a v u tm)
    (+ (* .5 a tm tm) (* v tm) u)
)

;; you need to complete this procedure, then show some test cases

; (pos 0 0 0 0)
; (pos 0 0 20 0)
; (pos 0 5 10 10)
; (pos 2 2 2 2)
; (pos 5 5 5 5)


;; Problem 2

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

;; complete these procedures and show some test cases

;; Problem 3

(defun time-to-impact (vertical-velocity elevation)
    (setq a (* 0.5 GRAVITY (- 1)))
    (setq r1 (root1 a vertical-velocity elevation))
    (setq r2 (root2 a vertical-velocity elevation))

    (if (>= r1 0) r1 r2)
)

;; Note that if we want to know when the ball drops to a particular height r 
;; (for receiver), we have

(defun time-to-height (vertical-velocity elevation target-elevation)
    (time-to-impact vertical-velocity (- elevation target-elevation))
)

;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(defun degree2radian (deg)
    (/ (*  deg pi) 180.)
)

(defun travel-distance-simple (elevation velocity angle)
    (YOUR-CODE-HERE)
)

;; let's try this out for some example values.  Note that we are going to 
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(defun meters-to-feet (m)
    (/ (* m 39.6) 12)
)

(defun feet-to-meters (f)
    (/ (* f 12) 39.6)
)

(defun hours-to-seconds (h)
    (* h 3600)
)

(defun seconds-to-hours (s)
    (/ s 3600)
)

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
;; at an angle of (/ pi 4) radians or 45 degrees

;; what is the distance traveled in each case?
;; record both in meters and in feet


;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(defun alpha-increment 0.01)

(defun find-best-angle (velocity elevation)
    (YOUR-CODE-HERE)
)

;; find best angle
;; try for other velocities
;; try for other heights
