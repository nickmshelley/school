#lang s-exp "language.rkt"

beta -> 30
start-angle -> 90
length -> .08
generations -> 5
axiom -> (F)
F -> .33 => F \[ + F \] F \[ − F \] F
     .33 => F \[ + F \] F
     .34 => F \[ - F \] F