#lang s-exp "language.rkt"

beta -> 25.7
start-angle -> 90
length -> .025
generations -> 7
axiom -> (X)
X -> 1 => F \[ + X \] \[ - X \] F X
F -> 1 => F F

;beta -> 30
;start-angle -> 90
;length -> .08
;generations -> 5
;axiom -> (F)
;F -> .33 => F \[ + F \] F \[ − F \] F
;     .33 => F \[ + F \] F
;     .34 => F \[ - F \] F

;beta -> 22.5
;start-angle -> 90
;length -> .1
;generations -> 4
;axiom -> (F)
;F -> 1 => F F - \[ - F + F + F \] + \[ + F - F - F \]