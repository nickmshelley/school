#lang s-exp "language.rkt"

beta -> 27
start-heading -> (0 1 0)
start-up -> (0 0 1)
radius -> .02
length -> .1
generations -> 5
axiom -> F
F -> .33 => F [+ F] F [− F] F
     .33 => F [+ F] F
     .34 => F [- F] F