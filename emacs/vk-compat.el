;; Copyright (C) 2008  Vesa Karvonen

(unless (string-match "XEmacs" emacs-version)
  (defalias 'line-number (function line-number-at-pos)))

(provide 'vk-compat)
