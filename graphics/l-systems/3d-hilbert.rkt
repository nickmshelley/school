#lang s-exp "language.rkt"

beta -> 90
start-heading -> (0 0 -1)
start-up -> (0 1 0)
radius -> .02
length -> .1
generations -> 3
axiom -> A
A -> 1 => B - F + C F C + F - D & F ^ D - F + & & C F C + F + B > >
B -> 1 => A & F ^ C F B ^ F ^ D ^ ^ - F - D ^ ! F ^ B ! F C ^ F ^ A > >
C -> 1 => ! D ^ ! F ^ B - F + C ^ F ^ A & & F A & F ^ C + F + B ^ F ^ D > >
D -> 1 => ! C F B - F + B ! F A & F ^ A & & F B - F + B ! F C > >