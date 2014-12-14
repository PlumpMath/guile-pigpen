(define-module (pigpen encoder)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 regex)
  #:export (encode char->pigpen))

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
     "     "
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
     " ╲ ╱ "
     "  ╳  ")
    (#\t
     " ╲   "
     "  ╳  "
     " ╱   ")
    (#\u
     "   ╱ "
     "  ╳  "
     "   ╲ ")
    (#\v
     "  ╳  "
     " ╱ ╲ "
     "╱   ╲")
    (#\w
     "╲   ╱"
     " ╲o╱ "
     "  ╳  ")
    (#\x
     " ╲   "
     " o╳  "
     " ╱   ")
    (#\y
     "   ╱ "
     "  ╳o "
     "   ╲ ")
    (#\z
     "  ╳  "
     " ╱o╲ "
     "╱   ╲")
    (#\space
     "     "
     "     "
     "     ")))

(define (char->pigpen ch)
  (let ((pigpen-symbol (assoc-ref %ascii-mapping (char-downcase ch))))
    (if pigpen-symbol
        pigpen-symbol
        (list
         "     "
         (string-append "  " (string ch) "  ")
         "     "))))

(define (string->pigpen-list str)
  (let ((res '()))
    (string-for-each
     (lambda (ch)
       (set! res (cons (char->pigpen ch) res)))
     str)
    (reverse res)))

(define (display/pigpen pigpen-list)
  (for-each (lambda (e)
              (display (car e)))
            pigpen-list)
  (newline)
  (for-each (lambda (e)
              (display (cadr e)))
            pigpen-list)
  (newline)
  (for-each (lambda (e)
              (display (caddr e)))
            pigpen-list)
  (newline))

(define* (encode str #:key (output-format 'ascii))
  (let ((pigpen-list (string->pigpen-list str)))
    ;; (display pigpen-list)
    ;; (newline)
    (display/pigpen pigpen-list)))
