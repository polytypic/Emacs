(provide 'text-flow)

(defgroup text-flow nil
  "The text-flow package aims to make it easier to manage blocks of text.

  Copyright (C) 2002  Vesa Karvonen

  This program is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
  Public License for more details.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write to the Free Software Foundation, Inc.,
  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA."
  :group 'editing)

(defcustom text-flow-max-column 74
  "Maximum line length for text flow."
  :type '(integer)
  :group 'text-flow)

(defun text-flow-paragraph (min max &optional max-column)
  "Reflows the text to fit in the max-column.

`max-column' defaults to `text-flow-max-column', but you can also give it
as a prefix argument (e.g. C-u 50 M-x text-flow-paragraph)."
  (interactive "*r\nP")
  (if (not max-column)
      (setq max-column text-flow-max-column))
  (let* ((string
          (buffer-substring min max))
         (ends-in-newline
          (equal ?\n (aref string (- (length string) 1))))
         (string
          (if ends-in-newline
              string
            (concat string "\n")))
         (prefix
          (text-flow-common-line-prefix string)))
    (delete-region min max)
    (goto-char min)
    (text-flow-generate (text-flow-without-line-prefix string
                                                       (length prefix)
                                                       -1)
                        prefix
                        (concat "\n"
                                (make-string (current-column) ?\ ))
                        max-column
                        0
                        t)
    (when ends-in-newline
      (insert "\n"))))

(defun text-flow-generate (string prefix suffix max-column i0 first)
  (if i0
      (let* ((i1
              (string-match "[ \n\t]" string i0))
             (i1
              (if i1
                  i1
                (length string)))
             (next-i0
              (string-match "[^ \n\t]" string i1)))
        (cond (first
               (insert prefix (substring string i0 i1))
               (text-flow-generate string prefix suffix max-column next-i0 nil))
              ((< (+ (- i1 i0) (current-column)) max-column)
               (insert " " (substring string i0 i1))
               (text-flow-generate string prefix suffix max-column next-i0 nil))
              (t
               (insert suffix)
               (text-flow-generate string prefix suffix max-column i0 t))))))

(defun text-flow-without-line-prefix (string prefix-length line-end)
  (if (or (not line-end)
          (< (length string) (+ line-end 1 prefix-length)))
      string
    (let* ((line-start
            (+ line-end 1))
           (string
            (concat (substring string 0 line-start)
                    (substring string (+ line-start prefix-length)))))
      (text-flow-without-line-prefix string
                                     prefix-length
                                     (string-match "\n" string line-start)))))

(defun text-flow-common-line-prefix (string &optional i0 prefix)
  (if (not i0)
      (let ((i1 (string-match "\n" string)))
        (if (and i1 (< (+ i1 1)
                       (length string)))
            (progn
              (setq i0 (+ i1 1))
              (setq prefix (substring string 0 i1)))
          (setq i0 (length string))
          (setq prefix ""))))
  (if (< i0 (length string))
      (let ((i1
             (string-match "\n" string i0)))
        (text-flow-common-line-prefix string
                                      (+ i1 1)
                                      (text-flow-common-prefix prefix
                                                               (substring string i0 i1))))
    prefix))

(defun text-flow-common-prefix (lhs rhs &optional i)
  (if (not i)
      (setq i 0))
  (if (and (< i (length lhs))
           (< i (length rhs))
           (equal (aref lhs i) (aref rhs i)))
      (text-flow-common-prefix lhs rhs (+ i 1))
    (substring lhs 0 i)))
