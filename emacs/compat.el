;; Copyright (C) 2008  Vesa Karvonen

(defalias 'compat-line-number
  (if (string-match "XEmacs" emacs-version)
      (function line-number)
    (function line-number-at-pos)))
