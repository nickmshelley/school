#lang s-exp "language.rkt"

beta -> 22.5
start-heading -> (0 1 0)
start-up -> (0 0 1)
radius -> .02
length -> .1
generations -> 5
axiom -> X
X -> 1 => F [+ X] [- X] F X
F -> 1 => F F