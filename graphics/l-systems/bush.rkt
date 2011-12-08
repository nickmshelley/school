#lang s-exp "language.rkt"

beta -> 22.5
start-heading -> (0 1 0)
start-up -> (0 0 1)
radius -> .01
length -> .1
generations -> 4
axiom -> F - F
F -> 1 => F F - [- F + F + F] + [+ F - F - F]