;;; string.scm -- Pigpen strings.

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

(define-module (pigpen string)
  #:use-module (srfi   srfi-1)
  #:use-module (srfi   srfi-26)
  #:use-module (pigpen char)
  #:export (pigpen-string
            pigpen-string?
            string->pigpen-string
            pigpen-string->string
            list->pigpen-string
            pigpen-string->list
            string-for-each/pigpen
            string-fold-right/pigpen))


(define <pigpen-string>
  (make-vtable "prpr"
               (lambda (struct port)
                 (format port "#<pigpen-string ~a>" (struct-ref struct 0)))))

(define (pigpen-string? x)
  "Check if X is a <pigpen-string> instance."
  (and (struct? x)
       (eq? (struct-vtable x) <pigpen-string>)))


(define (list->pigpen-string lst)
  "Convert a list LST to a pigpen string."
  (make-struct/no-tail <pigpen-string>
                       (list->string (map pigpen-char->char lst))
                       lst))

(define (pigpen-string->list pstr)
  "Convert a pigpen string PSTR to a list of pigpen characters."
  (if (pigpen-string? pstr)
      (struct-ref pstr 1)
      (error "Wrong type (expecting a pigpen string)" pstr)))


(define (string-for-each/pigpen proc pstr)
  "PROC is mapped over PSTR in left-to-right order."
  (for-each proc (pigpen-string->list pstr)))

(define (string-fold-right/pigpen proc knil pstr)
  "Apply PROC to a pigpen string PSTR to build a result, and return
that result."
  (fold proc knil (pigpen-string->list pstr)))


(define (pigpen-string->string pstr)
  "Convert a pigpen string PSTR to a scheme string."
  (let append-line ((idx 0)
                    (res ""))
    (if (< idx 3)
        (append-line
         (1+ idx)
         (string-fold-right/pigpen (lambda (elem prev)
                                     (string-append
                                      prev
                                      (list-ref (pigpen-char->list elem) idx)))
                                   (string-append res "\n")
                                   pstr))
        res)))

(define (string->pigpen-string str)
  "Convert a string STR to a pigpen string."

  (define (parse-pigpen-char lines)
    (list->pigpen-char (list (string-take (list-ref lines 0) 5)
                             (string-take (list-ref lines 1) 5)
                             (string-take (list-ref lines 2) 5))))

  (let parse ((lines (string-split str #\newline))
              (plist '()))
    (if (not (string-null? (car lines)))
        (let ((pchar (parse-pigpen-char lines)))
          (parse (list (string-drop (list-ref lines 0) 5)
                       (string-drop (list-ref lines 1) 5)
                       (string-drop (list-ref lines 2) 5))
                 (cons pchar plist)))
        (list->pigpen-string (reverse plist)))))

;;; string.scm ends here
