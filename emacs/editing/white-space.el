;; Functions for editing white-space in Emacs.
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

(require 'vk-compat)

(defun white-space-for-each-line (operation)
  "Performs the operation on every line. The operation should not move the
cursor beyond the line."
  (let ((old-line (line-number)))
    (beginning-of-buffer)
    (while (not (eobp))
      (funcall operation)
      (forward-line))
    (goto-line old-line)))

(defun white-space-remove-at-all-sol ()
  "Removes white-space at all start-of-lines"
  (interactive)
  (white-space-for-each-line
   (lambda ()
     (beginning-of-line)
     (while (and (not (eobp)) (or (equal ?\  (char-after)) (equal ?\t (char-after))))
       (delete-char 1)))))

(defun white-space-remove-at-all-eol ()
  "Removes white-space at all end-of-lines"
  (interactive)
  (white-space-for-each-line
   (lambda ()
     (end-of-line)
     (while (and (not (bobp)) (or (equal ?\  (char-before)) (equal ?\t (char-before))))
       (delete-backward-char 1)))))

(defun white-space-trim ()
  "Removes redundant white-space in the region or if region is not defined, after the point."
  (interactive)
  (if (and (not (white-space-is (char-before))) (not (eobp)))
      (forward-char))
  (if (region-exists-p)
      (let ((begin (if (< (region-beginning) (region-end)) (region-beginning) (region-end)))
            (end (if (< (region-beginning) (region-end)) (region-end) (region-beginning))))
        (goto-char begin)
        (while (< (point) end)
          (if (and
               (white-space-is (char-before))
               (white-space-is (char-after)))
              (progn
                (white-space-smart-delete-char)
                (setq end (- end 1)))
            (forward-char))))
    (while (and (not (eobp)) (white-space-is (char-after)))
      (white-space-smart-delete-char))))

(defun white-space-eat ()
  "Eats all white-space between the point and the first non-white-space character."
  (interactive)
  (while (and (not (eobp)) (white-space-is (char-after)))
    (delete-char 1)))

(defun white-space-smart-delete-char ()
  "Removes the next white-space character preserving one line break and converting tabs to spaces."
  (let ((case (string (char-before) (char-after))))
    (backward-char)
    (insert (cond
             ((string= case "\ \ ") "\ ")
             ((string= case "\ \t") "\ ")
             ((string= case "\ \n") "\n")
             ((string= case "\t\ ") "\ ")
             ((string= case "\t\t") "\ ")
             ((string= case "\t\n") "\n")
             ((string= case "\n\ ") "\n")
             ((string= case "\n\t") "\n")
             ((string= case "\n\n") "\n")
             (t (forward-char) (error 'error "Point must be surrounded by white-space."))))
    (delete-char 2)))

(defun white-space-is (c)
  "Returns true if the character is white-space"
  (or (equal c ?\t) (equal c ?\ ) (equal c ?\n)))

(provide 'white-space)
