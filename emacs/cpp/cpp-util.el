;; Functions for editing C preprocessor code.
;; Copyright (C) 2003  Vesa Karvonen
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

(require 'cc-mode)
(provide 'cpp-util)

(defun cpp-remove-backslashes-region (min max)
  "Calls: `c-backslash-region' to remove backslashes in the region."
  (interactive "r")
  (c-backslash-region min max t))

(defun cpp-indent-and-add-backslashes-region (min max)
  "Calls:
- `cpp-remove-backslashes-region',
- `indent-region', and
- `c-backslash-region'
to reindent the region and and backslashes."
  (interactive "r")
  (let ((lines (count-lines min max)))
    (cpp-remove-backslashes-region min max)
    (indent-region min max nil)
    (goto-char min)
    (forward-line lines)
    (c-backslash-region min (point) nil)))
