;;; char.scm -- Pigpen charachters.

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

(define-module (pigpen char)
  #:use-module (srfi   srfi-26)
  #:use-module (pigpen cipher)
  #:export (pigpen-char
            pigpen-char?
            char->pigpen))


(define <pigpen-char>
  (make-vtable "prpr"
               (lambda (struct port)
                 (format port "#<pigpen-char ~a>" (struct-ref struct 0)))))

(define (pigpen-char? x)
  "Check if X is a <pigpen-char> instance."
  (and (struct? x)
       (eq? (struct-vtable x) <pigpen-char>)))


(define (char->pigpen ch)
  "Convert a char CH to the corresponding pigpen symbol."
  (cond ((not (char? ch))
         (error "Wrong type (expecting char)" ch))
        ((char=? ch #\nul)
         (make-struct/no-tail <pigpen-char> ch (list "" "" "")))
        ((assoc-ref %ascii-mapping (char-downcase ch)) =>
         (cut make-struct/no-tail <pigpen-char> ch <>))
        (else
         (make-struct/no-tail <pigpen-char> ch
                              (list
                               "     "
                               (string-append "  " (string ch) "  ")
                               "     ")))))

;;; char.scm ends here
