;;; init --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Setup PATH variable on OS X

(require 'subr-x)

(let ((path-file "~/.path.txt"))
  (when (file-exists-p path-file)
    (let* ((shell-path (split-string
                        (with-temp-buffer
                          (insert-file-contents path-file)
                          (string-trim (buffer-string)))
                        ":"))
           (old-path (split-string (getenv "PATH") ":"))
           (new-path (reduce (lambda (path dir)
                               (if (member dir path)
                                   path
                                 (cons dir path)))
                             (cons old-path (reverse shell-path))))
           (new-exec-path (reduce (lambda (path dir)
                                    (if (member dir path)
                                        path
                                      (cons dir path)))
                                  (cons exec-path (reverse shell-path)))))
      (setq exec-path new-exec-path)
      (setenv "PATH" (string-join new-path ":")))))

;; Paths for personal packages

(let ((dirs (list
             "emacs/buffers"
             "emacs/editing"
             "emacs/util"
             "emacs/test"
             "emacs")))
  (mapc (lambda (d)
          (add-to-list 'load-path (file-truename (concat init-el-dir d))))
        dirs))

;; Some personal keybindings

(global-set-key [(control delete)] 'kill-region)

(global-set-key [(control tab)] (lambda () (interactive) (other-window -1)))

(global-set-key [(meta right)] 'forward-sexp)
(global-set-key [(meta left)] 'backward-sexp)
(global-set-key [(meta up)] 'backward-paragraph)
(global-set-key [(meta down)] 'forward-paragraph)

;; Configure packages

(require 'use-package)

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package buffers ;; My package
  :commands (indent-buffer)
  :bind (("C-c k" . buffers-kill-current)))

(use-package white-space ;; My package
  :commands (white-space-remove-at-all-sol
             white-space-remove-at-all-eol)
  :bind (([(control backspace)] . white-space-eat)))

(use-package magit
  :ensure t
  :commands (magit magit-blame-popup))

(use-package clang-format
  :ensure t
  :commands (clang-format clang-format-buffer)
  :init
  (add-hook
   'before-save-hook
   (lambda ()
     (when (derived-mode-p 'c++-mode)
       (clang-format-buffer)))))

(use-package ffap
  :ensure t
  :bind (("C-x C-f" . find-file-at-point)))

(use-package fill-column-indicator
  :ensure t
  :commands (fci-mode)
  :hook ((1ml-mode fom-mode sh-mode c-mode c++-mode cmake-mode markdown-mode menhir-mode reason-mode) . fci-mode))

(use-package multiple-cursors
  :ensure t
  :bind (([(control meta left)] . mc/mark-previous-like-this)
         ([(control meta right)] . mc/mark-next-like-this)
         ([(shift control meta left)] . mc/skip-to-previous-like-this)
         ([(shift control meta right)] . mc/skip-to-next-like-this)))

(use-package move-text
  :ensure t
  :bind (([(control meta down)] . move-text-down)
         ([(control meta up)] . move-text-up)))

(use-package markdown-mode
  :ensure t
  :commands (gfm-mode)
  :mode ("\\.md\\'" . gfm-mode))

(use-package prettier-js
  :ensure t
  :commands (prettier-js-mode)
  :hook ((css-mode less-css-mode markdown-mode rjsx-mode typescript-mode web-mode)
         . prettier-js-mode))

(use-package rjsx-mode
  :ensure t
  :mode ("\\.js\\'" . rjsx-mode))

(use-package flycheck
  :ensure t)

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode))

(use-package flycheck-rust
  :ensure t)

(use-package cmake-mode :ensure t)

(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package cmake-ide :ensure t :config (cmake-ide-setup))

(use-package auto-complete-clang
  :ensure t
  :hook (c++-mode . auto-complete-mode))

(use-package rtags :ensure t)

(use-package yaml-mode :ensure t)

(use-package csharp-mode :ensure t)
(use-package fsharp-mode :ensure t)

(use-package expand-region
  :ensure t
  :bind (("C-+" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package irony :ensure t
  :hook (c++-mode . irony-mode))

(use-package irony-eldoc
  :ensure t
  :hook (irony-mode . irony-eldoc))

(use-package eldoc-cmake
  :ensure t
  :hook (cmake-mode . eldoc-cmake-enable))

(use-package haskell-mode :ensure t)

(use-package kotlin-mode :ensure t)
(use-package flycheck-kotlin :ensure t)

(use-package reason-mode :ensure t)

(use-package company :ensure t)
(use-package iedit :ensure t)

(use-package tuareg :ensure t)
(use-package merlin :ensure t)
(use-package ocamlformat :ensure t
  :custom (ocamlformat-enable 'enable-outside-detected-project)
  :hook (before-save . ocamlformat-before-save))
(use-package flycheck-ocaml :ensure t)

(use-package sml-mode :ensure t)

(use-package 1ml-mode
  :load-path "~/Projects/1ml/1ml-mode/src"
  :mode ("\\.1ml\\'" . 1ml-mode)
  :init
  (add-hook
   '1ml-mode-hook (lambda () (setq-local flycheck-checker '1ml))))

(use-package web-mode :ensure t
  :mode ("\\.html\\'" . web-mode))

(use-package auto-highlight-symbol :ensure t)

(use-package go-mode :ensure t)

(use-package vterm :ensure t)

(use-package dockerfile-mode :ensure t)
(use-package docker-compose-mode :ensure t)

(provide 'init)
;;; init.el ends here
