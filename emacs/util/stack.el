;; Stack like list manipulation for elisp.
;; Copyright (C) 2002  Vesa Karvonen
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc., 59
;; Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(provide 'stack)

;; Macro interface

(defmacro stack-push-q (list x)
  "Adds x to the head of the list variable."
  `(progn
     (assert (symbolp ',list))
     (assert (listp ,list))
     (setq ,list (cons ,x ,list))))

(defmacro stack-pop-q (list)
  "Pops an element from the head of the list variable and returns it.
If list is already `nil', then `nil' is returned."
  (let ((result (make-symbol "result")))
    `(progn
       (assert (symbolp ',list))
       (assert (listp ,list))
       (let ((,result (car ,list)))
         (setq ,list (cdr ,list))
         ,result))))

;; Function interface

(defun stack-push (list-symbol x)
  "Adds x to the head of the list variable."
  (assert (symbolp list-symbol))
  (assert (listp (eval list-symbol)))
  (set list-symbol (cons x (eval list-symbol))))

(defun stack-pop (list-symbol)
  "Pops an element from the head of the list variable and returns it.
If list is already `nil', then `nil' is returned."
  (assert (symbolp list-symbol))
  (let ((l (eval list-symbol)))
    (assert (listp l))
    (let ((result (car l)))
      (set list-symbol (cdr l))
      result)))
