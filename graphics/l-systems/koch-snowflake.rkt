#lang s-exp "language.rkt"

beta -> 60
start-heading -> (1 1 0)
start-up -> (0 0 1)
radius -> .02
length -> .1
generations -> 3
axiom -> F + + F + + F
F -> 1 => F - F + + F - F
