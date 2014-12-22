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
  #:use-module (srfi   srfi-1)
  #:use-module (srfi   srfi-26)
  #:use-module (pigpen cipher)
  #:export (pigpen-char
            pigpen-char?
            pigpen-char=?
            char->pigpen-char
            pigpen-char->char
            pigpen-char->list
            list->pigpen-char
            pigpen-char->string
            string->pigpen-char))


(define <pigpen-char>
  (make-vtable "prpr"
               (lambda (struct port)
                 (format port "#<pigpen-char ~a>" (struct-ref struct 0)))))

(define (pigpen-char? x)
  "Check if X is a <pigpen-char> instance."
  (and (struct? x)
       (eq? (struct-vtable x) <pigpen-char>)))


(define (pigpen-char=? a b)
  "Check if pigpen char A and pigpen char B are equal."
  (char=? (pigpen-char->char a)
          (pigpen-char->char b)))


(define (char->pigpen-char ch)
  "Convert a char CH to the corresponding pigpen symbol."
  (cond ((not (char? ch))
         (error "Wrong type (expecting char)" ch))
        ((char=? ch #\nul)
         (make-struct/no-tail <pigpen-char> ch (list "" "" "")))
        ((assoc-ref %unicode-mapping (char-downcase ch)) =>
         (cut make-struct/no-tail <pigpen-char> ch <>))
        (else
         (make-struct/no-tail <pigpen-char> ch
                              (list
                               "     "
                               (string-append "  " (string ch) "  ")
                               "     ")))))

(define (pigpen-char->char pch)
  "Convert a pigpen char PCH to the corresponding character."
  (if (pigpen-char? pch)
      (struct-ref pch 0)
      (error "Wrong type (expecting pigpen char)" pch)))


(define (pigpen-char->list pch)
  "Convert a pigpen char PCH to a list of strings."
  (if (pigpen-char? pch)
      (struct-ref pch 1)
      (error "Wrong type (expecting pigpen char)" pch)))

(define (list->pigpen-char lst)
  "Convert list of strings LST to a pigpen character.  LST is expected
to be a list of 3 elements."
  (if (= (length lst) 3)
      (let ((m (find (lambda (e)
                       (let ((l (cdr e)))
                         (and (string=? (list-ref l 0) (list-ref lst 0))
                              (string=? (list-ref l 1) (list-ref lst 1))
                              (string=? (list-ref l 2) (list-ref lst 2)))))
                     %unicode-mapping)))
        (if m
            (make-struct/no-tail <pigpen-char> (car m) lst)
            (let ((ch (string-trim (list-ref lst 1))))
              (if (not (string-null? ch))
                  (make-struct/no-tail <pigpen-char> (string-ref ch 0) lst)
                  (error "Could not convert the list" lst)))))
      (error "Wrong number of lines (expecting 3)" lst)))


(define (pigpen-char->string pch)
  (if (pigpen-char? pch)
      (let ((pchar-list (struct-ref pch 1)))
        (string-append (list-ref pchar-list 0) "\n"
                       (list-ref pchar-list 1) "\n"
                       (list-ref pchar-list 2)))
      (error "Wrong type (expecting pigpen char)" pch)))

(define (string->pigpen-char str)
  "Convert a string STR to a pigpen character."
  (list->pigpen-char (string-split str #\newline)))

;;; char.scm ends here
