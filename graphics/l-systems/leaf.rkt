#lang s-exp "language.rkt"

beta -> 22.5
start-angle -> 90
length -> .025
generations -> 7
axiom -> (X)
X -> 1 => F \[ + X \] \[ - X \] F X
F -> 1 => F F
