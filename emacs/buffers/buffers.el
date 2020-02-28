;; Functions for manipulating buffers in XEmacs.
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

(defun buffers-kill-current ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer nil))

(defun buffers-switch-to-next ()
  "Switches to the next buffer cyclically."
  (interactive)
  (switch-to-other-buffer 0))

;; TBD: buffers.el isn't perhaps the right place for this function.
(defun indent-buffer ()
  "Indents current buffer according to current indent mode, see `indent-according-to-mode' function."
  (interactive)
  (indent-region (point-min)
		 (point-max)
		 nil))

(provide 'buffers)
