;; -*- mode: emacs-lisp; encoding: utf-8; -*-

;; Run custom first, so that the rest of the initialization process
;; can use what it sets.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" default)))
 '(ecb-layout-name "right1")
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.25)
 '(ido-create-new-buffer (quote always))
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode t)
 '(package-archive-priorities (quote (("gnu" . 2) ("melpa-stable" . 1))))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (ecb dockerfile-mode apropospriate-theme zenburn-theme yaml-mode web-mode toml-mode smex smartparens ruby-tools ruby-end racer projectile paredit-menu markdown-mode magit js2-mode ido-ubiquitous hideshowvis haml-mode glsl-mode flycheck es-mode cmake-mode elisp--witness--lisp company cider ag paredit use-package)))
 '(safe-local-variable-values
   (quote
    ((cider-boot-parameters . "cider dev repl -s wait")
     (encoding . utf-8))))
 '(server-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(use-package-always-ensure t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; package.el initialization; bootstrap to make sure the use-package
;; package is installed.
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Package configurations.

(use-package ag)

(use-package apropospriate-theme)

(use-package cider
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-mode-hook 'company-mode)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode))

(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'eldoc-mode)
  (add-hook 'clojure-mode-hook 'company-mode)
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)))

(use-package cmake-mode
  :mode "CMakeLists")

(use-package company
  :config
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'ielm-mode-hook 'company-mode))

(use-package dockerfile-mode
  :mode "\\`Dockerfile")

(use-package ecb
  :commands ecb-activate
  :ensure nil
  :pin melpa)

(use-package es-mode)

(use-package flycheck)

(use-package glsl-mode)

(use-package haml-mode)

(use-package hideshowvis)

(use-package ido-ubiquitous)

(use-package js2-mode
  :config
  (add-hook 'js2-mode-hook 'smartparens-mode)
  (add-hook 'js2-mode-hook 'flycheck-mode))

(use-package magit
  :commands magit-status
  :bind (("C-x g" . magit-status))
  :config
  (add-hook 'git-commit-setup-hook (lambda () (auto-fill-mode -1)))
  (add-hook 'git-commit-setup-hook 'visual-line-mode))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (add-hook 'markdown-mode-hook 'visual-line-mode))

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'ielm-mode-hook 'paredit-mode))

(use-package paredit-menu)

(use-package projectile
  :config
  (projectile-global-mode 1))

(use-package racer)

(use-package ruby-end
  :diminish ruby-end-mode)

(use-package ruby-tools
  :diminish ruby-tools-mode)

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'rust-mode-hook 'company-mode))

(use-package smartparens
  :init
  (add-hook 'c++-mode-hook 'smartparens-mode))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(use-package toml-mode
  :mode "Cargo\\.lock\\'")

(use-package web-mode)

(use-package yaml-mode
  :mode "\\.yml\\'")

;; Hooks for built-in things to built-in things.
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

;; ibuffer is nicer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Swap out the regular isearches for their regexp counterparts.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Something I do way too often.
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)

;; Initialize colors for screen / tmux
(add-to-list 'load-path "~/.emacs.d/user-lisp")

;; Load any .el files in user-lisp.
(dolist (f (directory-files "~/.emacs.d/user-lisp" t "\\.el\\'")) (load f))
