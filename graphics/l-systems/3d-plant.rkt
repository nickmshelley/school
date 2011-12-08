#lang s-exp "language.rkt"

beta -> 22.5
start-heading -> (0 1 0)
start-up -> (0 0 1)
radius -> .01
length -> .1
generations -> 6
axiom -> A
A -> 1 => [& F L A] > > > > >[& F L A] > > > > > > > [& F L A]
F -> 1 => S > > > > > F
S -> 1 => F L
L -> 1 => [^ ^ - F + F + F - ! - F + F + F]