#lang s-exp "language.rkt"

beta -> 45
start-angle -> 45
length -> .025
generations -> 7
axiom -> (X)
X -> 1 => F \[ + X \] \[ - X \] F X
F -> 1 => F F
