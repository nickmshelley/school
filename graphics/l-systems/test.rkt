#lang s-exp "language.rkt"
(2d #:beta 22.5
    #:start-angle 90
    #:length .05
    (F))
(F -> 0.33 => F [+ F] F [- F] F
      0.33 => F [+ F] F
      0.34 => F [- F] F)


;beta -> 22.5
;start-angle -> 90
;length -> .05
;axiom -> F
;F .33 -> F [ + F ] F [ - F ] F
;  .33 -> F [ + F ] F
;  .34 -> F [ - F ] F
