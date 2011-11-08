#lang s-exp "language.rkt"

beta -> 22.5
start-angle -> 90
length -> .05
generations -> 5
axiom -> (F)
F -> 1 => F F - \[ - F + F + F \] + \[ + F - F - F \]