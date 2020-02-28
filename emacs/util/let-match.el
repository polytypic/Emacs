;; Simple pattern matching let for Emacs Lisp.
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

(provide 'let-match)

(defun let-match-gen-bindings (fetch-expr pattern result)
  (cond
   ((null pattern) result)
   ((consp pattern) (let-match-gen-bindings
                     `(cdr ,fetch-expr)
                     (cdr pattern)
                     (let-match-gen-bindings
                      `(car ,fetch-expr)
                      (car pattern) result)))
   ((symbolp pattern) (cons
                       (list pattern fetch-expr)
                       result))
   (t (error 'error "Unsupported pattern: " pattern))))

(defmacro let-match (bindings &rest body-exprs)
  (let ((value-sym (make-symbol "target"))
        (value-pattern (caar bindings))
        (value-expr (cadar bindings)))
    `(let ((,value-sym ,value-expr))
       (let ,(let-match-gen-bindings value-sym value-pattern nil)
         ,@body-exprs))))
