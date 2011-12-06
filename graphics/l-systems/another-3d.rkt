#lang s-exp "language.rkt"

beta -> 28
start-heading -> (1 1 0)
start-up -> (0 0 1)
length -> .1
generations -> 3
axiom -> F
F -> 1 => F [ & + F] F [ - > F][+ > F] [^ F]