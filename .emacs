
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(create-lockfiles nil)
 '(cua-mode t nil (cua-base))
 '(desktop-save-mode t)
 '(eldoc-idle-delay 0.25)
 '(fill-column 80)
 '(flycheck-clang-language-standard "c++17")
 '(flycheck-clang-warnings '("all" "extra" "no-pragma-once-outside-header"))
 '(flycheck-global-modes t)
 '(fsharp-indent-level 2)
 '(fsharp-indent-offset 2)
 '(global-auto-revert-mode t)
 '(global-flycheck-mode t)
 '(global-linum-mode t)
 '(grep-command "git ls-files -z | xargs -0 grep  -nH --null -e ")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js2-strict-missing-semi-warning nil)
 '(mac-command-modifier nil)
 '(mac-option-modifier 'meta)
 '(mac-right-option-modifier nil)
 '(magit-log-section-commit-count 100)
 '(make-backup-files nil)
 '(markdown-fontify-code-blocks-natively t)
 '(mc/always-run-for-all t)
 '(ns-alternate-modifier 'meta)
 '(ns-command-modifier nil)
 '(ns-right-alternate-modifier 'none)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(typescript-mode company-lsp yasnippet lsp-ui lsp-metals sbt-mode lsp-mode scala-mode purescript-mode fsharp-mode elm-mode csharp-mode flycheck-ocaml sml-mode tuareg reason-mode flycheck-kotlin kotlin-mode haskell-mode eldoc-cmake irony-eldoc irony expand-region expand-refion rtags cmake-font-lock yaml-mode auto-complete-clang cmake-ide cmake-mode lsp-rust flycheck-rust rust-mode flycheck rjsx-mode rjsx prettier-js markdown-mode auto-package-update fill-column-indicator move-text multiple-cursors mc-mark-more clang-format magit use-package))
 '(ring-bell-function 'ignore)
 '(rust-format-on-save t)
 '(rust-indent-offset 4)
 '(safe-local-variable-values
   '((eval progn
           (setq flycheck-clang-include-path
                 (let
                     ((base
                       (expand-file-name
                        (locate-dominating-file
                         (buffer-file-name)
                         ".dir-locals.el"))))
                   (list
                    (concat base "libs/utility/include")
                    (concat base "libs/platform/intel/include")
                    (concat base "libs/mcs/include")
                    (concat base "libs/reagents/include")))))
     (eval progn
           (setq flycheck-clang-include-path
                 (let
                     ((base
                       (expand-file-name
                        (locate-dominating-file
                         (buffer-file-name)
                         ".dir-locals.el"))))
                   (list
                    (concat base "libs/platform/intel/include")
                    (concat base "libs/mcs/include")
                    (concat base "libs/reagents/include")))))
     (eval progn
           (setq flycheck-clang-include-path
                 (let
                     ((base
                       (expand-file-name
                        (locate-dominating-file
                         (buffer-file-name)
                         ".dir-locals.el"))))
                   (list
                    (concat base "libs/mcs/include")
                    (concat base "libs/reagents/include")))))
     (eval progn
           (setq flycheck-clang-include-path
                 (let
                     ((base
                       (expand-file-name
                        (locate-dominating-file
                         (buffer-file-name)
                         ".dir-locals.el"))))
                   (list
                    (concat base "libs/mcs/inc")
                    (concat base "libs/reagents/inc")))))
     (eval progn
           (setq flycheck-clang-include-path
                 (let
                     ((base
                       (expand-file-name
                        (locate-dominating-file
                         (buffer-file-name)
                         ".dir-locals.el"))))
                   (list
                    (concat base "libs/reagents/inc")))))
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name
             (concat
              (locate-dominating-file
               (buffer-file-name)
               ".dir-locals.el")
              "src"))))
     (eval progn
           (setq flycheck-clang-include-path
                 (let
                     ((base
                       (expand-file-name
                        (locate-dominating-file
                         (buffer-file-name)
                         ".dir-locals.el"))))
                   (list
                    (concat base "inc")))))))
 '(scroll-conservatively 100)
 '(scroll-margin 2)
 '(sh-basic-offset 2)
 '(show-paren-mode t)
 '(sml-electric-pipe-mode nil)
 '(sml-indent-level 2)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "nil" :family "Menlo"))))
 '(def-use-def-face ((t (:background "darkseagreen4"))))
 '(def-use-mark-face ((t (:background "orchid4"))))
 '(def-use-unused-def-face ((t (:background "deeppink4"))))
 '(def-use-use-face ((t (:background "paleturquoise4"))))
 '(def-use-view-face ((t (:background "chocolate4"))))
 '(font-lock-comment-face ((t (:foreground "chocolate4"))))
 '(font-lock-constant-face ((t (:foreground "Aquamarine3")))))

(require 'cl)
(setq init-el-dir "/Users/polytypic/Projects/Emacs/")
(load (concat init-el-dir "init.el"))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
