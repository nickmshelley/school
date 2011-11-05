#lang s-exp "language.rkt"

;beta -> 22.5
;start-angle -> 90
;length -> .05
;axiom -> F
;F .33 -> F [ + F ] F [ - F ] F
;  .33 -> F [ + F ] F
;  .34 -> F [ - F ] F

(l-system 22.5 ;beta
          90 ;start-angle
          .05 ;length
          5
          '(F) ;axiom
          (productions '(('F '((.33 (F \[ + F \] F \[ − F \] F))
                               (.33 (F \[ + F \] F))
                               (.34 (F \[ - F \] F)))))))