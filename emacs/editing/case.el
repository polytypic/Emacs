;; Functions for dealing with character case.
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

(provide 'case)

(defun case-change-region (start end &optional buffer)
  "Changes the case of each character in the region."
  (interactive "r")
  (if (< end start)
      (case-change-region end start buffer)
    (let ((old-point (point)))
      (goto-char start)
      (while (< (point) end)
        (let ((changed (case-change-char (char-after) buffer)))
          (delete-char)
          (insert-char changed)))
      (goto-char old-point))))

(defun case-change-char (char &optional buffer)
  "Changes the case of the give character"
  (let ((up (upcase char buffer)))
    (if (char= char up)
        (downcase char buffer)
      up)))
