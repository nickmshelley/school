#lang s-exp "language.rkt"

beta -> 90
start-heading -> (1 1 0)
start-up -> (0 0 1)
length -> .01
generations -> 3
axiom -> A
A -> 1 => B - F + C F C + F - D & F ^ D - F + & & C F C + F + B > >
B -> 1 => A & F ^ C F B ^ F ^ D ^ ^ - F - D ^ ! F ^ B ! F C ^ F ^ A > >
C -> 1 => ! D ^ ! F ^ B - F + C ^ F ^ A & & F A & F ^ C + F + B ^ F ^ D > >
D -> 1 => ! C F B - F + B ! F A & F ^ A & & F B - F + B ! F C > >