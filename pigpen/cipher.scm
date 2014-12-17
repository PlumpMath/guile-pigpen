(define-module (pigpen cipher)
  #:use-module (ice-9 optargs)
  #:export (print-substitution-table
            %ascii-mapping
            %substitution-table))

(define %ascii-mapping
  '((#\a
     "     "
     "    │"
     "━━━━┙")
    (#\b
     "     "
     "│   │"
     "┕━━━┙")
    (#\c
     "     "
     "│    "
     "┕━━━━")
    (#\d
     "━━━━┑"
     "    │"
     "━━━━┙")
    (#\e
     "┍━━━┑"
     "│   │"
     "┕━━━┙")
    (#\f
     "┍━━━━"
     "│    "
     "┕━━━━")
    (#\g
     "━━━━┑"
     "    │"
     "     ")
    (#\h
     "┍━━━┑"
     "│   │"
     "     ")
    (#\i
     "┍━━━━"
     "│    "
     "     ")
    (#\j
     "     "
     "  o │"
     "━━━━┙")
    (#\k
     "     "
     "│ o │"
     "┕━━━┙")
    (#\l
     "     "
     "│ o  "
     "┕━━━━")
    (#\m
     "━━━━┑"
     "  o │"
     "━━━━┙")
    (#\n
     "┍━━━┑"
     "│ o │"
     "┕━━━┙")
    (#\o
     "┍━━━━"
     "│ o  "
     "┕━━━━")
    (#\p
     "━━━━┑"
     "  o │"
     "     ")
    (#\q
     "┍━━━┑"
     "│ o │"
     "     ")
    (#\r
     "┍━━━━"
     "│ o  "
     "     ")
    (#\s
     "╲   ╱"
     "╲╲ ╱╱"
     " ╲╳╱ ")
    (#\t
     " ╲╲  "
     "  ╳  "
     " ╱╱  ")
    (#\u
     "  ╱╱ "
     "  ╳  "
     "  ╲╲ ")
    (#\v
     " ╱╳╲ "
     "╱╱ ╲╲"
     "╱   ╲")
    (#\w
     "╲   ╱"
     "╲╲o╱╱"
     " ╲╳╱ ")
    (#\x
     " ╲╲  "
     " o╳  "
     " ╱╱  ")
    (#\y
     "  ╱╱ "
     "  ╳o "
     "  ╲╲ ")
    (#\z
     " ╱╳╲ "
     "╱╱o╲╲"
     "╱   ╲")
    (#\-
     "     "
     "━━━━━"
     "     ")
    (#\space
     "     "
     "     "
     "     ")))


(define %substitution-table
  "
A     B     C     J     K     L
    │   │           o │ o │ o  
━━━━┿━━━┿━━━━     ━━━━┿━N━┿━━━━
D   │ E │   F     M o │ o │ o O
━━━━┿━━━┿━━━━     ━━━━┿━━━┿━━━━
    │   │           o │ o │ o  
G     H     I     P     Q     R
                               
      S                 W      
    ╲   ╱             ╲   ╱    
    ╲╲ ╱╱             ╲╲o╱╱    
     ╲╳╱               ╲╳╱     
  ╲╲     ╱╱         ╲╲     ╱╱  
T  ╳     ╳  U     X o╳     ╳o Y
  ╱╱     ╲╲         ╱╱     ╲╲  
     ╱╳╲               ╱╳╲     
    ╱╱ ╲╲             ╱╱o╲╲    
    ╱   ╲             ╱   ╲    
      V                 Z      
")

(define* (print-substitution-table #:optional (port (current-output-port)))
  (display %substitution-table port))

;;; cipher.scm ends here
