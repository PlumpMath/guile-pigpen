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
  #:use-module (pigpen char)
  #:use-module (pigpen cipher)
  #:use-module (pigpen string)
  #:export (encode))

(define* (encode str #:key (format 'ascii))
  "Convert a string STR to a pigpen list."
  (let* ((cipher  (get-cipher format))
         (pch-nul (char->pigpen-char #\nul (car cipher))))
    (list->pigpen-string
     (string-fold-right (lambda (ch prev)
                          (cons (char->pigpen-char ch (car cipher)) prev))
                        (list pch-nul)
                        str))))

;;; encoder.scm ends here
