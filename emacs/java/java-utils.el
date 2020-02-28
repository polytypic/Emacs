;; XEmacs utility functions for editing Java code.
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

(require 'jde-import)
(provide 'java-utils)

(defconst java-utils-id-pattern "[a-zA-Z_][a-zA-Z0-9_]*")
(defconst java-utils-import-pattern (concat "^import "
                                            "\\(" java-utils-id-pattern "[.]\\)*"
                                            java-utils-id-pattern
                                            "\\([.][*]\\)?;"))

(defun java-utils-sort-imports ()
  "Sorts the import declarations at the start of the file."
  (interactive)
  (let ((old-point (point)))
    (let ((first-import
           (progn
             (goto-char (point-min))
             (re-search-forward java-utils-import-pattern)
             (beginning-of-line)
             (point)))
          (last-import
           (progn
             (goto-char (point-max))
             (re-search-backward java-utils-import-pattern)
             (end-of-line)
             (point))))
      (goto-char old-point)
      (if (<= last-import first-import)
          (error 'error "Error finding imports")
        (sort-lines nil first-import last-import)))))

(defun java-utils-import-find-and-import-and-sort-imports (class)
  "Imports the class (using `jde-import-find-and-import') and then
sorts the imports if the buffer didn't change."
  (interactive
   (list (read-from-minibuffer "Class: "
                               (thing-at-point 'symbol))))
  (let ((old-buffer (current-buffer)))
    (jde-import-find-and-import class)
    (if (eq old-buffer (current-buffer))
        (java-utils-sort-imports))))
