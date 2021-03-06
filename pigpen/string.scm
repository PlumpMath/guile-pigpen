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
  #:use-module (ice-9  receive)
  #:use-module (pigpen cipher)
  #:use-module (pigpen char)
  #:export (pigpen-string
            pigpen-string?
            pigpen-string:cipher-name
            make-pigpen-string
            string->pigpen-string
            pigpen-string->string
            list->pigpen-string
            pigpen-string->list
            string-for-each/pigpen
            string-fold-right/pigpen
            pigpen-string-split
            fill))


(define <pigpen-string>
  (make-vtable "prpr"
               (lambda (struct port)
                 (format port "#<pigpen-string ~a ~a>"
                         (struct-ref struct 0)
                         (number->string (object-address struct) 16)))))

(define (pigpen-string? x)
  "Check if X is a <pigpen-string> instance."
  (and (struct? x)
       (eq? (struct-vtable x) <pigpen-string>)))

(define (pigpen-string:cipher-name pstr)
  (pigpen-char:cipher-name (car (pigpen-string->list pstr))))

(define (make-pigpen-string plaintext pchars)
  "Make a pigpen string of PCHARS that represents a PLAINTEXT."
  (make-struct/no-tail <pigpen-string> plaintext pchars))


(define (list->pigpen-string lst)
  "Convert a list LST to a pigpen string."
  (make-pigpen-string (list->string (map pigpen-char->char lst)) lst))

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


(define (pigpen-string-split pstr pch)
  "Split a pinpen string PSTR using a pinpen char PCH as the
delimiter.  Return list of pinpen strings."
  (let ((lst        (pigpen-string->list pstr))
        (delimiter? (cut pigpen-char=? <> pch)))
    (let split ((l   lst)
                (res '()))
      (receive (head tail)
          (break delimiter? l)
        (if (and (not (null? head))
                 (not (null? tail)))
            (split (drop tail 1) (cons head res))
            (map (cut list->pigpen-string <>)
                 (reverse (cons head res))))))))


(define (pigpen-string->string pstr)
  "Convert a pigpen string PSTR to a scheme string."

  (define (append-line pstr idx res)
    (if (< idx 3)
        (append-line pstr
                     (1+ idx)
                     (string-fold-right/pigpen
                      (lambda (elem prev)
                        (string-append prev
                                       (list-ref (pigpen-char->list elem)
                                                 idx)))
                      (string-append res "\n")
                      pstr))
        res))

  (let ((cipher (get-cipher (pigpen-string:cipher-name pstr))))
    (fold (lambda (elem prev)
            (string-append prev (append-line elem 0 "")))
          ""
          (pigpen-string-split pstr (char->pigpen-char #\newline cipher)))))

(define (string->pigpen-string str cipher)
  "Convert a string STR to a pigpen string."

  (define (parse-pigpen-char lines)
    (list->pigpen-char (list (string-take (list-ref lines 0) 5)
                             (string-take (list-ref lines 1) 5)
                             (string-take (list-ref lines 2) 5))
                       cipher))

  (let parse ((lines (string-split str #\newline))
              (plist '()))
    (if (not (string-null? (car lines)))
        (let ((pchar (parse-pigpen-char lines)))
          (parse (list (string-drop (list-ref lines 0) 5)
                       (string-drop (list-ref lines 1) 5)
                       (string-drop (list-ref lines 2) 5))
                 (cons pchar plist)))
        (list->pigpen-string (reverse plist)))))


(define* (fill pstr #:key (columns 75))
  (let* ((lst            (pigpen-string->list pstr))
         (pchar-width    (exact->inexact (pigpen-char:width (car lst))))
         (cipher         (get-cipher (pigpen-string:cipher-name pstr)))
         (limit          (inexact->exact (round (/ columns pchar-width))))
         (pigpen-newline (char->pigpen-char #\newline cipher)))
    (let f ((l   lst)
            (res '()))
      (if (> (length l) limit)
          (f (drop l limit)
             (append res (append (take l limit) (list pigpen-newline))))
          (list->pigpen-string (append res l))))))

;;; string.scm ends here
