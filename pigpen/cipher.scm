(define-module (pigpen cipher)
  #:use-module (ice-9 optargs)
  #:export (pigpen-cipher
            pigpen-cipher?
            make-pigpen-cipher
            get-cipher-name
            get-cipher-alphabet
            get-cipher-substable
            add-cipher!
            get-cipher
            print-substitution-table))

(define %ciphers '())

(define <pigpen-cipher>
  (make-vtable "prprpr"
               (lambda (struct port)
                 (format port "#<pigpen-cipher ~a ~a>"
                         (struct-ref struct 0) ;name
                         (number->string (object-address struct) 16)))))

(define (pigpen-cipher? x)
  "Check if X is a <pigpen-cipher> instance."
  (and (struct? x)
       (eq? (struct-vtable x) <pigpen-cipher>)))

(define (make-pigpen-cipher name alphabet substitution-table)
  (make-struct/no-tail <pigpen-cipher> name alphabet substitution-table))

(define (get-cipher-name cipher)
  (if (pigpen-cipher? cipher)
      (struct-ref cipher 0)
      (error "Wrong type (expecting a pigpen cipher)" cipher)))

(define (get-cipher-alphabet cipher)
  (if (pigpen-cipher? cipher)
      (struct-ref cipher 1)
      (error "Wrong type (expecting a pigpen cipher)" cipher)))

(define (get-cipher-substable cipher)
  (if (pigpen-cipher? cipher)
      (struct-ref cipher 2)
      (error "Wrong type (expecting a pigpen cipher)" cipher)))


(define (add-cipher! name cipher)
  (set! %ciphers (acons name cipher %ciphers)))

(define (get-cipher name)
  "Get a cipher by NAME."
  (assoc-ref %ciphers name))


(define* (print-substitution-table #:key
                                   (fmt  'ascii)
                                   (port (current-output-port)))
  (let ((cipher (get-cipher fmt)))
    (if cipher
        (display (get-cipher-substable cipher) port)
        (error "Unknown cipher type" fmt))))


;;; Ciphers

(add-cipher!
 'unicode
 (make-pigpen-cipher
  'unicode
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
     "     "))
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
"))

(add-cipher!
 'ascii
 (make-pigpen-cipher
  'ascii
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
     "     "))
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
"))

(add-cipher!
 'none
 (make-pigpen-cipher
  'none
  '((#\a
     ".---."
     ".---|"
     "'---'")
    (#\b
     "|    "
     "|---."
     "'---'")
    (#\c
     ".--- "
     "|    "
     "'--- ")
    (#\d
     "    |"
     ".---|"
     "'---'")
    (#\e
     ".---."
     "|---'"
     "'--- ")
    (#\f
     ".----"
     "|--- "
     "'    ")
    (#\g
     ".---."
     "'---|"
     " ---'")
    (#\h
     "|    "
     "|---."
     "'   '")
    (#\i
     " _o  "
     "  |  "
     " -'- ")
    (#\j
     " _o  "
     "  |  "
     "'-'  ")
    (#\k
     "| _- "
     "|'   "
     "!`-. ")
    (#\l
     " -.  "
     "  |  "
     " -'- ")
    (#\m
     ".-.-."
     "| | |"
     "' ' '")
    (#\n
     ".---."
     "|   |"
     "'   '")
    (#\o
     ".---."
     "|   |"
     "'---'")
    (#\p
     ".---."
     "|---'"
     "'    ")
    (#\q
     ".---."
     "'---|"
     "    '")
    (#\r
     "|.--."
     "|    "
     "'    ")
    (#\s
     ".--- "
     "'---."
     " ---'")
    (#\t
     " |_  "
     " |   "
     " '-' ")
    (#\u
     ".   ."
     "|   |"
     "'---!")
    (#\v
     ".   ."
     "|   |"
     "'--' ")
    (#\w
     ". . ."
     "| | |"
     "'-'-'")
    (#\x
     ".-.-."
     "  |  "
     "'-'-'")
    (#\y
     "|   |"
     "'---|"
     "'---'")
    (#\z
     ".---."
     " .-' "
     "'---'")
    (#\.
     "  _   "
     " '_'  "
     "      ")
    (#\,
     "  _  "
     " '_| "
     "  -' ")
    (#\-
     "     "
     "-----"
     "     "))
  #f))

;;; cipher.scm ends here
