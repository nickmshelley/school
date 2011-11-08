#lang s-exp "language.rkt"

beta -> 20
start-angle -> 90
length -> .05
generations -> 5
axiom -> (X)
;F -> .33 => F \[ + F \] F \[ − F \] F
;      .33 => F \[ + F \] F
;      .34 => F \[ - F \] F
X -> 1 => F \[ + X \] \[ - X \] F X
F -> 1 => F F
