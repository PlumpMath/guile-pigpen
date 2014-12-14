;;; encoder.scm -- Pigpen encoder.

;; Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; TODO


;;; Code:

(define-module (pigpen encoder)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 regex)
  #:use-module (srfi  srfi-1)
  #:use-module (pigpen cipher)
  #:export (encode
            char->pigpen
            string->pigpen-list
            pigpen-list->pigpen-string))

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
  "Convert a string STR to a pigpen list."
  (string-fold-right (lambda (ch prev)
                       (cons (char->pigpen ch) prev))
                     (list (list "" "" ""))
                     str))

(define (pigpen-list->pigpen-string lst)
  "Convert a pigpen list LST to a pigpen string."
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

(define* (display/pigpen pigpen-list #:optional (port (current-output-port)))
  "Display a PIGPEN-LIST."
  (display (pigpen-list->pigpen-string pigpen-list) port))

(define* (encode str #:key (output-format 'ascii))
  (let ((pigpen-list (string->pigpen-list str)))
    ;; (display pigpen-list)
    ;; (newline)
    (display/pigpen pigpen-list)))

;;; encoder.scm ends here
