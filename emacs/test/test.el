;; Test framework for elisp.
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

(require 'stack)

(provide 'test)

(defmacro test-cases (&rest tests)
  "Runs a list of test cases of the form (msg-string invariant-expr)."
  (let ((errors (make-symbol "errors")))
    `(let ((,errors nil))
       ,@(mapcar
          (lambda (test-case)
            `(if (not ,(cadr test-case))
                 (stack-push-q ,errors ,(car test-case) )))
          tests)
       (if (not ,errors)
           (message "OK")
         (error 'error (reverse ,errors))))))
