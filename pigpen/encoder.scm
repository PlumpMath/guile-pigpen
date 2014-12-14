(define-module (pigpen encoder)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 regex)
  #:use-module (srfi  srfi-1)
  #:export (encode
            char->pigpen
            string->pigpen-list
            pigpen-list->pigpen-string))

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

(define (char->pigpen ch)
  "Convert a char CH to the corresponding pigpen symbol."
  (let ((pigpen-symbol (assoc-ref %ascii-mapping (char-downcase ch))))
    (if pigpen-symbol
        pigpen-symbol
        (list
         "     "
         (string-append "  " (string ch) "  ")
         "     "))))

(define (string->pigpen-list str)
  (string-fold-right (lambda (ch prev)
                       (cons (char->pigpen ch) prev))
                     (list (list "" "" ""))
                     str))

(define (pigpen-list->pigpen-string lst)
  (let append-line ((idx 0)
                    (res ""))
    (if (< idx 3)
        (append-line
         (1+ idx)
         (fold (lambda (elem prev)
                 (string-append prev (list-ref elem idx)))
               (string-append res "\n")
               lst))
        res)))

(define (display/pigpen pigpen-list)
  (display (pigpen-list->pigpen-string pigpen-list)))

(define* (encode str #:key (output-format 'ascii))
  (let ((pigpen-list (string->pigpen-list str)))
    ;; (display pigpen-list)
    ;; (newline)
    (display/pigpen pigpen-list)))
