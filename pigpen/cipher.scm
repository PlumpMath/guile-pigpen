(define-module (pigpen cipher)
  #:use-module (ice-9 optargs)
  #:export (print-substitution-table
            get-cipher
            %unicode-mapping
            %substitution-table
            %ascii-mapping
            %ascii-substitution-table))

(define %unicode-mapping
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

(define %ascii-mapping
  '((#\a
     "    ."
     "    |"
     "----'")
    (#\b
     ".   ."
     "|   |"
     "'---'")
    (#\c
     ".    "
     "|    "
     "'----")
    (#\d
     "----."
     "    |"
     "----'")
    (#\e
     ".---."
     "|   |"
     "'---'")
    (#\f
     ".----"
     "|    "
     "'----")
    (#\g
     "----."
     "    |"
     "    '")
    (#\h
     ".---."
     "|   |"
     "'   '")
    (#\i
     ".----"
     "|    "
     "'    ")
    (#\j
     "    ."
     "  o |"
     "----'")
    (#\k
     ".   ."
     "| o |"
     "'---'")
    (#\l
     ".    "
     "| o  "
     "'----")
    (#\m
     "----."
     "  o |"
     "----'")
    (#\n
     ".---."
     "| o |"
     "'---'")
    (#\o
     ".----"
     "| o  "
     "'----")
    (#\p
     "----."
     "  o |"
     "----'")
    (#\q
     ".---."
     "| o |"
     ".   .")
    (#\r
     ".----"
     "| o  "
     "'    ")
    (#\s
     "\\   /"
     " [ ] "
     "  V  ")
    (#\t
     "\\    "
     " [ ]>"
     "/    ")
    (#\u
     "    /"
     "<[ ] "
     "    \\")
    (#\v
     "  A  "
     " [ ] "
     "/   \\")
    (#\w
     "\\   /"
     " [o] "
     "  V  ")
    (#\x
     "\\    "
     " [o]>"
     "/    ")
    (#\y
     "    /"
     "<[o] "
     "    \\")
    (#\z
     "  A  "
     " [o] "
     "/   \\")
    (#\-
     "     "
     "-----"
     "     ")
    (#\space
     "     "
     "     "
     "     ")))


(define %ascii-substitution-table
  "
A   |  B  |   C   J   |  K  |   L
    |     |         o |  o  | o  
----+-----+----   ----+--N--+----
D   |  E  |   F   M o |  o  | o O
----+-----+----   ----+-----+----
    |     |         o |  o  | o  
G   |  H  |   I   P   |  Q  |   R
                                 
       S                 W       
     \\   /             \\   /     
      [ ]               [o]      
       V                 V       
 \\           /     \\           / 
T [ ]>   <[ ] U   X [o]>   <[o] Y
 /           \\     /           \\ 
       A                 A       
      [ ]               [o]      
     /   \\             /   \\     
       V                 Z       
")


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

(define %ciphers
  `((ascii   ,%ascii-mapping   ,%ascii-substitution-table)
    (unicode ,%unicode-mapping ,%substitution-table)))

(define* (print-substitution-table #:key
                                   (fmt  'ascii)
                                   (port (current-output-port)))
  (cond ((eq? fmt 'unicode)
         (display %substitution-table port))
        ((eq? fmt 'ascii)
         (display %ascii-substitution-table port))
        (else
         (error "Unknown format" fmt))))

(define (get-cipher name)
  "Get a cipher by NAME."
  (assoc-ref %ciphers name))

;;; cipher.scm ends here
